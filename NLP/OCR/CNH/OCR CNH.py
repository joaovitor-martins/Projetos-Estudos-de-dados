import cv2
import pytesseract

# Define o caminho para o executável do Tesseract
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'

# Caminho para a imagem webp
imagem_path = r'C:\Users\marti_ezke8vn\Downloads\VSCode\NLP\Projeto OCR - CNH\cnh.webp'

# Função para extrair texto de uma região específica da imagem
def extrair_texto_regiao(imagem, coordenadas):
    x, y, w, h = coordenadas
    regiao = imagem[y:y+h, x:x+w]
    texto = pytesseract.image_to_string(regiao, lang='por')
    return texto.strip()

# Função para salvar a região da imagem
def salvar_regiao(imagem, coordenadas, nome_arquivo):
    x, y, w, h = coordenadas
    regiao = imagem[y:y+h, x:x+w]
    cv2.imwrite(nome_arquivo, regiao)

# Carregar a imagem
imagem = cv2.imread(imagem_path)

# Extrair texto das regiões especificadas
nome = extrair_texto_regiao(imagem, (155, 145, 320, 22))
numero_doc = extrair_texto_regiao(imagem, (415, 195, 100, 30))
rg = extrair_texto_regiao(imagem, (163, 474, 200, 29))
cpf = extrair_texto_regiao(imagem, (415, 250, 175, 30))
dt_nascimento = extrair_texto_regiao(imagem, (600, 240, 120, 40))
tipo_habilitacao = extrair_texto_regiao(imagem, (650, 415, 100, 40))
dt_validade = extrair_texto_regiao(imagem, (413, 474, 130, 29))
dt_emissao = extrair_texto_regiao(imagem, (582, 820, 125, 29))
observacao = extrair_texto_regiao(imagem, (150, 548, 600, 200))
assinatura = extrair_texto_regiao(imagem, (254, 748, 230, 38))

# Salvar a foto do documento
salvar_regiao(imagem, (155, 230, 220, 200), 'foto_documento.jpg')

# Imprimir os resultados
print("Nome:", nome)
print("Número do documento e órgão:", numero_doc)
print("RG:", rg)
print("CPF:", cpf)
print("Data de nascimento:", dt_nascimento)
print("Tipo de habilitação:", tipo_habilitacao)
print("Data de validade:", dt_validade)
print("Data de emissão:", dt_emissao)
print("Observação:", observacao)
print("Assinatura:", assinatura)
