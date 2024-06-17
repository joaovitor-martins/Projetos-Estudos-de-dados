# -*- coding: utf-8 -*-
"""NLP - Sprint 1.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1BElVSa1vm-DTB0LcvfZE-rk4PIR-LdjU

## Importando bibliotecas e o dataset

### Biblioteca
"""

import pandas             as pd
import matplotlib.pyplot  as plt

import spacy

from sklearn.model_selection          import train_test_split, GridSearchCV, cross_val_score
from sklearn.feature_extraction.text  import TfidfVectorizer
from sklearn.naive_bayes              import MultinomialNB
from sklearn.pipeline                 import Pipeline
from collections                      import Counter
from wordcloud                        import WordCloud

# Carregar modelo spaCy para português
!python -m spacy download pt_core_news_sm
nlp = spacy.load('pt_core_news_sm')

"""### Dataset's"""

# Carregar datasets
df = pd.read_csv('/content/Base Sprint 1.csv', delimiter=';')
df_teste = pd.read_csv('/content/Base teste Sprint1.csv', delimiter=';')

"""## Analise preditiva do dataset

### Dataset de treino
"""

df.sample(5)

df.info()

df.describe()

"""### Dataset de validação"""

df_teste.sample(5)

df_teste.info()

df_teste.describe()

"""## Modelo"""

def processamento(texto):
    doc = nlp(texto.lower())
    tokens = [token.lemma_ for token in doc if token.is_alpha and not token.is_stop]
    return ' '.join(tokens)

# Pré-processar os comentários
df['processado'] = df['comentario'].apply(processamento)
df_teste['processado'] = df_teste['comentario'].apply(processamento)

# Dividir os dados de treino e teste
X_train, X_val, y_train, y_val = train_test_split(df['processado'], df['sentimento'], test_size=0.2, random_state=42)

# Criar o pipeline de classificação
pipeline = Pipeline([
    ('tfidf', TfidfVectorizer()),
    ('clf', MultinomialNB())
])

# Hiperparâmetros para Grid Search
param_grid = {
    'tfidf__max_df': [0.75, 1.0],
    'tfidf__ngram_range': [(1, 1), (1, 2)],
    'clf__alpha': [0.5, 1.0]
}

# Grid Search com validação cruzada
grid_search = GridSearchCV(pipeline, param_grid, cv=5)
grid_search.fit(X_train, y_train)

print(f'Melhores parâmetros: {grid_search.best_params_}')
pipeline = grid_search.best_estimator_

# Avaliar no conjunto de validação
acuracia = pipeline.score(X_val, y_val)
print(f'Acuracia: {acuracia:.2f}')

# Classificar os comentários do dataset de teste
X_test = df_teste['processado']
predicoes = pipeline.predict(X_test)

# Adicionar as previsões ao dataframe de teste
df_teste['sentimento_predito'] = predicoes

"""## Visualização"""

# Análise e visualização
estatisticas = df_teste['sentimento_predito'].value_counts()
comentarios_repetidos = Counter(df_teste['comentario']).most_common()

servico_positivo = df_teste[df_teste['sentimento_predito'] == 'positivo']['servico'].mode()[0]
servico_negativo = df_teste[df_teste['sentimento_predito'] == 'negativo']['servico'].mode()[0]
servico_mais_comentado = df_teste['servico'].mode()[0]

sentimentos_por_servico = df_teste.groupby(['servico', 'sentimento_predito']).size().unstack().fillna(0)
servico_mais_positivo = sentimentos_por_servico['positivo'].idxmax()
servico_mais_negativo = sentimentos_por_servico['negativo'].idxmax()

print(f'Serviço com mais comentários: {servico_mais_comentado}')
print(f'Serviço mais positivo: {servico_mais_positivo}')
print(f'Serviço mais negativo: {servico_mais_negativo}')

# Contar a frequência de cada comentário para os sentimentos positivos e negativos
comentarios_positivos = df_teste[df_teste['sentimento_predito'] == 'positivo']['comentario']
comentarios_negativos = df_teste[df_teste['sentimento_predito'] == 'negativo']['comentario']

comentarios_positivos_contagem = Counter(comentarios_positivos)
comentarios_negativos_contagem = Counter(comentarios_negativos)

# Obter os 3 comentários mais frequentes para cada classe de sentimento
comentarios_positivos_relevantes = comentarios_positivos_contagem.most_common(3)
comentarios_negativos_relevantes = comentarios_negativos_contagem.most_common(3)

# Criar as tabelas de comentários mais relevantes
comentarios_positivos_df = pd.DataFrame(comentarios_positivos_relevantes, columns=['Comentário', 'Quantidade'])
comentarios_positivos_df['Sentimento'] = 'Positivo'
comentarios_positivos_df['Acurácia do Modelo'] = acuracia

comentarios_negativos_df = pd.DataFrame(comentarios_negativos_relevantes, columns=['Comentário', 'Quantidade'])
comentarios_negativos_df['Sentimento'] = 'Negativo'
comentarios_negativos_df['Acurácia do Modelo'] = acuracia

# Exibir as tabelas
print("Tabela de Comentários Mais Relevantes - Positivos:")
comentarios_positivos_df

print("\nTabela de Comentários Mais Relevantes - Negativos:")
comentarios_negativos_df

fig, axes = plt.subplots(2, 2, figsize=(15, 12))

sentimentos_por_servico.plot(kind='bar', ax=axes[0, 0], rot=45)
axes[0, 0].set_title('Sentimentos por Serviço')
axes[0, 0].set_ylabel('Quantidade de Comentários')

df_teste[df_teste['sentimento_predito'] == 'positivo']['servico'].value_counts().plot(kind='bar', ax=axes[0, 1])
axes[0, 1].set_title('Serviço mais Positivo')
axes[0, 1].set_xlabel('Serviço')
axes[0, 1].set_ylabel('Quantidade de Comentários')

for i in axes[0, 1].patches:
    axes[0, 1].text(i.get_x() + i.get_width() / 2, i.get_height(), str(i.get_height()), ha='center', va='bottom')

df_teste[df_teste['sentimento_predito'] == 'negativo']['servico'].value_counts().plot(kind='bar', ax=axes[1, 0])
axes[1, 0].set_title('Serviço mais Negativo')
axes[1, 0].set_xlabel('Serviço')
axes[1, 0].set_ylabel('Quantidade de Comentários')

for i in axes[1, 0].patches:
    axes[1, 0].text(i.get_x() + i.get_width() / 2, i.get_height(), str(i.get_height()), ha='center', va='bottom')

axes[1, 1].barh([f'{comment[:20]}...' for comment, _ in comentarios_repetidos[:5]], [count for _, count in comentarios_repetidos[:5]])
axes[1, 1].set_title('Comentários mais Relevantes')
axes[1, 1].set_xlabel('Quantidade de Ocorrências')

plt.subplots_adjust(wspace=0.5, hspace=0.5)
plt.tight_layout()
plt.show()

# Separar os comentários positivos e negativos
comentarios_positivos = df_teste[df_teste['sentimento_predito'] == 'positivo']['processado']
comentarios_negativos = df_teste[df_teste['sentimento_predito'] == 'negativo']['processado']

# Função para gerar nuvem de palavras baseada na relevância TF-IDF
def gerar_nuvem_de_palavras(comentarios):
    tfidf = TfidfVectorizer(max_df=0.75, ngram_range=(1, 2))
    tfidf_matrix = tfidf.fit_transform(comentarios)
    sum_tfidf = tfidf_matrix.sum(axis=0)
    palavras_tfidf = [(word, sum_tfidf[0, idx]) for word, idx in tfidf.vocabulary_.items()]
    palavras_tfidf = sorted(palavras_tfidf, key=lambda x: x[1], reverse=True)
    palavras_tfidf_dict = {word: score for word, score in palavras_tfidf}
    wordcloud = WordCloud(width=800, height=400, background_color='white').generate_from_frequencies(palavras_tfidf_dict)
    return wordcloud

# Função para extrair palavras mais importantes de cada classe
def palavras_importantes_por_sentimento(pipeline, comentarios, top_n=50):
    tfidf = pipeline.named_steps['tfidf']
    clf = pipeline.named_steps['clf']

    # Transformar os comentários em uma matriz TF-IDF
    tfidf_matrix = tfidf.transform(comentarios)

    # Obter a pontuação média de TF-IDF para cada palavra
    sum_tfidf = tfidf_matrix.sum(axis=0)

    # Obter os índices das palavras com os maiores valores TF-IDF
    palavras_tfidf = [(word, sum_tfidf[0, idx]) for word, idx in tfidf.vocabulary_.items()]
    palavras_tfidf = sorted(palavras_tfidf, key=lambda x: x[1], reverse=True)[:top_n]

    palavras_tfidf_dict = {word: score for word, score in palavras_tfidf}
    return palavras_tfidf_dict

# Gerar nuvem de palavras para os comentários positivos
palavras_positivas = palavras_importantes_por_sentimento(pipeline, comentarios_positivos)
wordcloud_positivo = WordCloud(width=800, height=400, background_color='white').generate_from_frequencies(palavras_positivas)

# Gerar nuvem de palavras para os comentários negativos
palavras_negativas = palavras_importantes_por_sentimento(pipeline, comentarios_negativos)
wordcloud_negativo = WordCloud(width=800, height=400, background_color='white').generate_from_frequencies(palavras_negativas)

# Plotar as nuvens de palavras
fig, axes = plt.subplots(1, 2, figsize=(20, 10))

axes[0].imshow(wordcloud_positivo, interpolation='bilinear')
axes[0].set_title('Nuvem de Palavras - Positivas')
axes[0].axis('off')

axes[1].imshow(wordcloud_negativo, interpolation='bilinear')
axes[1].set_title('Nuvem de Palavras - Negativas')
axes[1].axis('off')

plt.show()

