---
title:    "C: Buscando e substituindo texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que

Se você é um programador C experiente ou está apenas começando a aprender a linguagem, uma habilidade essencial que você precisa dominar é a substituição de texto. Essa técnica permite que você encontre e substitua uma determinada sequência de caracteres em um texto, tornando a edição de arquivos de código muito mais fácil e rápida. Neste artigo, vamos explorar como realizar essa tarefa usando a linguagem C.

## Como fazer

Para substituir texto em um arquivo usando C, vamos primeiro criar uma função simples que recebe três argumentos: um ponteiro para o arquivo que queremos editar, a sequência de caracteres que queremos substituir e a sequência de caracteres pela qual queremos substituir a primeira. Aqui está o código para a nossa função:

```
void substituir_texto(FILE *arquivo, char *antigo, char *novo){
   // Enquanto não atingirmos o final do arquivo
   while(!feof(arquivo)){
      // Tenta ler uma linha inteira do arquivo
      char linha[100];
      if(fgets(linha, 100, arquivo) != NULL){
         // Se a linha contém o texto antigo, substitui por novo
         if(strstr(linha, antigo) != NULL){
            // Encontra a posição do texto antigo na linha
            int indice = strstr(linha, antigo) - linha;
            // Copia a substring anterior ao texto antigo
            char substring1[indice];
            strncpy(substring1, linha, indice);
            // Copia a substring após o texto antigo
            char substring2[100-indice-strlen(antigo)];
            strcpy(substring2, linha+indice+strlen(antigo));
            // Combina as substrings com o texto novo no meio
            char linha_final[100];
            sprintf(linha_final, "%s%s%s", substring1, novo, substring2);
            // Substitui a linha original pela nova linha no arquivo
            fseek(arquivo, -strlen(linha), SEEK_CUR);
            fputs(linha_final, arquivo);
         }
      }
   }
   // Fecha o arquivo ao final da substituição
   fclose(arquivo);
   printf("Texto substituído com sucesso.\n");
}
```

Vamos explicar linha por linha:

```
void substituir_texto(FILE *arquivo, char *antigo, char *novo){
```

Aqui, declaramos a função com seus argumentos. O primeiro argumento é um ponteiro para o arquivo que queremos editar. O segundo e o terceiro argumentos são as sequências de caracteres que queremos substituir e pela qual queremos substituir, respectivamente.

```
while(!feof(arquivo)){
```

Em seguida, temos um loop que verificará se atingimos o final do arquivo. Se não, o conteúdo do loop será executado.

```
if(fgets(linha, 100, arquivo) != NULL){
```

Dentro do loop, usamos a função `fgets()` para ler uma linha inteira do arquivo. Se a função retorna `NULL`, significa que atingimos o final do arquivo e o loop será interrompido.

```
if(strstr(linha, antigo) != NULL){
```

Agora, usamos a função `strstr()` para verificar se a linha contém a sequência de caracteres antiga que queremos substituir. Se a função retorna `NULL`, significa que a linha não contém a sequência e o processo de substituição é interrompido para essa linha.

```
int indice = strstr(linha, antigo) - linha;
```

Se a linha contém a sequência de caracteres antiga, usamos a função `strstr()` novamente para encontrar a posição dessa sequência na linha. A fórmula `strstr(linha, antigo) - linha` nos dá o índice da sequência de caracteres antiga dentro da linha.

```
char substring1[indice];
strncpy(substring1, linha, indice);
```

Em seguida, criamos uma substring que contém todos os caracteres antes da sequência de caracteres antiga.

```
char substring2[100-indice-strlen(antigo)];
strcpy(substring2, linha+indice+strlen(antigo));
```

Da mesma forma, criamos uma substring que contém todos os caracteres após a sequência de caracteres antiga.

```
char linha_final[100];
sprintf(linha_final, "%s%s%s", substring1, novo, substring2);
```

Usando a função `sprintf()`, combinamos as substrings com o texto novo no meio, criando assim a