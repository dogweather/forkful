---
title:                "Trabalhando com json"
html_title:           "PowerShell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com JSON, ou JavaScript Object Notation, é uma forma de armazenar e transmitir dados no formato de um objeto JavaScript. Programadores fazem isso porque JSON é uma maneira simples e eficiente de organizar informações, especialmente em aplicações web.

## Como fazer:

Para trabalhar com JSON em PowerShell, você pode usar o cmdlet `ConvertTo-Json` para converter objetos em formato JSON. Aqui está um exemplo de como converter um objeto em JSON:

```powershell
$exemplo = @{
    Nome = "João"
    Sobrenome = "Silva"
    Idade = 30
}

ConvertTo-Json $exemplo
```

Isso irá resultar no seguinte output:

```json
{
    "Nome": "João",
    "Sobrenome": "Silva",
    "Idade": 30
}
```

Para deserializar (converter de JSON para objeto) um dado JSON, você pode usar o cmdlet `ConvertFrom-Json`, conforme mostrado neste exemplo:

```powershell
$exemploJson = '{
    "Nome": "João",
    "Sobrenome": "Silva",
    "Idade": 30
}'

ConvertFrom-Json $exemploJson
```

O output será um objeto PowerShell contendo as propriedades e valores do JSON.

## Profundidade do assunto:

JSON foi criado inicialmente em 2001 por Douglas Crockford e se tornou amplamente usado como um formato de dados. Existem outras alternativas, como XML, mas JSON é mais popular por sua simplicidade e legibilidade humana. Para implementar o uso de JSON em sua aplicação, é preciso ter conhecimento sobre como converter e deserializar dados entre JSON e objetos em PowerShell.

## Veja também:

- [Documentação oficial do PowerShell sobre JSON](https://docs.microsoft.com/pt-br/powershell/scripting/learn/deep-dives/everything-about-json?view=powershell-7)
- [Tutorial em vídeo sobre trabalhar com JSON em PowerShell](https://www.youtube.com/watch?v=5o-AQoGNPWI)
- [Artigo em português sobre JSON e seus usos](https://renatogroffe.medium.com/como-manipular-json-usando-powershell-e1dda2569566)