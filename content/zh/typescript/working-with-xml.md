---
title:                "处理XML"
date:                  2024-01-26T04:36:32.345932-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
使用XML意味着通过编程解析、处理和编写XML数据。程序员处理XML是为了跨不同系统交换数据，用于配置文件，或者处理依赖XML的标准，如SOAP。

## 如何操作:
```TypeScript
import { parseString } from 'xml2js';

// 示例XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>别忘了会议!</body>
             </note>`;

// 将XML解析为JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// 假设解析成功，输出可能如下：
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['别忘了会议!'] } 
}
```

## 深入了解
XML，或可扩展标记语言，自90年代末期以来就存在了。它的自描述性质和人类可读的格式让它早期就在各种应用中大受欢迎，如RSS源、配置管理，甚至是办公文档格式，比如Microsoft Office Open XML。但是，与JSON相比，它较为冗长，潮流已经转变。JSON因其轻量级和与JavaScript的原生兼容性而在基于Web的APIs中获得了关注。

尽管如此，XML并没有消亡。它在大型企业系统和尚未转向JSON的文档标准中仍然被使用。如TypeScript的`xml2js`或Python的`lxml`等工具证明了在编程中仍然需要对XML进行操作。

TypeScript没有像对JSON那样对XML提供内置支持。相反，你需要使用库。`xml2js`就是一个例子。它将XML转换为JSON，使得数据对JavaScript高手来说更易于处理。

## 另见
- [MDN Web 文档关于XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm 包](https://www.npmjs.com/package/xml2js)
- [W3Schools XML 教程](https://www.w3schools.com/xml/)