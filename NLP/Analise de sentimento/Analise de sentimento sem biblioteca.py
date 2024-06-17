# -*- coding: utf-8 -*-
"""CP1  NLP.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1WrbaUEXgvCO8Xf8OvGjuYO7IJDzZwos2

**Aluno:** João Vitor de Andrade Martins

**RM:** 98744

Código de analise de sentimento sem usar bibliotecas
"""

# Dicionários de palavras
palavras_positivas = ["feliz", "bom", "alegre", "ótimo", "positivo"]
palavras_negativas = ["triste", "ruim", "deprimido", "horrível", "negativo"]

def adicionar_palavra(palavra, lista):
    lista.append(palavra.lower())  # Convertendo para minúsculas ao adicionar

def analise_sentimento(frase, palavras_positivas, palavras_negativas):
    # Separando por palavra
    palavras = frase.split()

    # Conta qtd de palavras positivas e negativas com base no dicionario
    contagem_positivas = sum(palavra in palavras_positivas for palavra in palavras)
    contagem_negativas = sum(palavra in palavras_negativas for palavra in palavras)

    if contagem_positivas > contagem_negativas:     # Frase positiva -> Mais palavras positivas
        return "Positivo"
    elif contagem_negativas > contagem_positivas:   # Frase negativa -> Mais palavras negativas
        return "Negativo"
    else:   # Frase neutra -> Sem palavras positivas e negativas
        return "Neutro"

# Menu
while True:
    print("\nMenu:")
    print("1. Analisar frase")
    print("2. Adicionar palavra")
    print("3. Sair")

    opcao = input("Escolha uma opção: ")

    if opcao == "1":
        frase = input("Digite uma frase para análise de sentimento: ").lower()  # Convertendo para minúsculas as palavras da frase
        sentimento = analise_sentimento(frase, palavras_positivas, palavras_negativas)
        print("Sentimento:", sentimento)
    elif opcao == "2":
        opcao_adicao = input("Escolha o dicionário para adicionar a palavra:\n1. Positivo\n2. Negativo\nEscolha uma opção: ")
        if opcao_adicao == "1":
            palavra = input("Digite a palavra positiva que deseja adicionar: ").lower() # Convertendo para minúsculas as palavras nova
            adicionar_palavra(palavra, palavras_positivas)
        elif opcao_adicao == "2":
            palavra = input("Digite a palavra negativa que deseja adicionar: ").lower() # Convertendo para minúsculas as palavras nova
            adicionar_palavra(palavra, palavras_negativas)
        else:
            print("Opção inválida!")
    elif opcao == "3":
        print("Encerrando o programa...")
        break
    else:
        print("Opção inválida!")

