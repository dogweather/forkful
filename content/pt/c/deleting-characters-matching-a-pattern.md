---
title:                "C: Excluindo caracteres que correspondem a um padrão"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Por que: Entendendo a eliminação de caracteres com um padrão

Às vezes, em um programa de computador, é necessário eliminar determinados caracteres de uma string que correspondam a um padrão específico. Pode ser uma tarefa aparentemente simples, mas entender por que essa ação é necessária pode ajudar a melhorar suas habilidades de programação. 

##Como Fazer: Exemplos de código e saída do programa

A seguir, mostraremos como implementar a função para eliminar os caracteres correspondentes a um padrão em uma string. Primeiro, declaremos uma string e um padrão para encontrar e excluir todos os caracteres correspondentes.

````C
#include <stdio.h>
#include <string.h>
 
// Função para excluir caracteres correspondentes a um padrão em uma string
void deleteCharacter(char str[], char pattern[]) 
{ 
    // Obtém o tamanho total da string
    int tamanho = strlen(str); 
    // Mantém contador para contar os caracteres a serem copiados
    int count = 0; 
  
    // Separa a string em partes e copia apenas os caracteres não correspondentes
    for (int i = 0; i < tamanho; i++) { 
        int j; 
        //Verifica se o caractere atual é diferente do padrão e copia para o índice de contador
        for (j = 0; j < strlen(pattern); j++) 
            if (str[i] == pattern[j]) 
                break; 
  
        //Se o caractere atual não for correspondente ao padrão, o copia 
        if (j == strlen(pattern)) 
            str[count++] = str[i]; 
    } 
    // Adiciona o caractere nulo ao final da string
    str[count] = '\0'; 
} 
  
//Função principal
int main() 
{ 
    //Exemplo de string e padrão
    char str[] = "Olá, mundo! Olá, mundo!"; 
    char pattern[] = "Oa"; 
  
    //Invoca a função que deleta os caracteres correspondentes da string
    deleteCharacter(str, pattern); 
  
    //Resultado esperado: "Ol, mund! Ol, mund!"
    printf("%s", str); 
    return 0; 
} 
````

##Profundidade: Mais informações sobre a eliminação de caracteres correspondentes a um padrão

Ao implementar o código acima, é importante notar que a função ```deleteCharacter``` não altera a string original. Em vez disso, ela cria uma nova string com os caracteres não correspondentes. Isso garante que a string original permaneça intacta.

Além disso, é possível criar uma função que aceite um ponteiro para ponteiro de string como argumento para evitar a criação de uma nova string. Isso também pode ser usado para excluir caracteres correspondentes de várias strings.

Outro ponto importante é que a função ```strlen ()``` pode ser lenta e, em vez disso, a função ```strlen (padrão)``` pode ser usada para armazenar o tamanho do padrão antes do loop para melhorar o desempenho.

##Veja também
- [Tutorial de Programação em C](https://www.programiz.com/c-programming)
- [Funções de String em C](https://www.geeksforgeeks.org/string-functions-in-c-with-examples/)
- [Documentação Oficial do C](https://devdocs.io/c/)