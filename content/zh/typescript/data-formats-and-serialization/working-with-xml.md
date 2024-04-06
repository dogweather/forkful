---
date: 2024-01-26 04:36:32.345932-07:00
description: "\u5982\u4F55\u64CD\u4F5C: XML\uFF0C\u6216\u53EF\u6269\u5C55\u6807\u8BB0\
  \u8BED\u8A00\uFF0C\u81EA90\u5E74\u4EE3\u672B\u671F\u4EE5\u6765\u5C31\u5B58\u5728\
  \u4E86\u3002\u5B83\u7684\u81EA\u63CF\u8FF0\u6027\u8D28\u548C\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u683C\u5F0F\u8BA9\u5B83\u65E9\u671F\u5C31\u5728\u5404\u79CD\u5E94\u7528\u4E2D\
  \u5927\u53D7\u6B22\u8FCE\uFF0C\u5982RSS\u6E90\u3001\u914D\u7F6E\u7BA1\u7406\uFF0C\
  \u751A\u81F3\u662F\u529E\u516C\u6587\u6863\u683C\u5F0F\uFF0C\u6BD4\u5982Microsoft\
  \ Office Open\u2026"
lastmod: '2024-04-05T21:53:47.822791-06:00'
model: gpt-4-0125-preview
summary: "XML\uFF0C\u6216\u53EF\u6269\u5C55\u6807\u8BB0\u8BED\u8A00\uFF0C\u81EA90\u5E74\
  \u4EE3\u672B\u671F\u4EE5\u6765\u5C31\u5B58\u5728\u4E86\u3002\u5B83\u7684\u81EA\u63CF\
  \u8FF0\u6027\u8D28\u548C\u4EBA\u7C7B\u53EF\u8BFB\u7684\u683C\u5F0F\u8BA9\u5B83\u65E9\
  \u671F\u5C31\u5728\u5404\u79CD\u5E94\u7528\u4E2D\u5927\u53D7\u6B22\u8FCE\uFF0C\u5982\
  RSS\u6E90\u3001\u914D\u7F6E\u7BA1\u7406\uFF0C\u751A\u81F3\u662F\u529E\u516C\u6587\
  \u6863\u683C\u5F0F\uFF0C\u6BD4\u5982Microsoft Office Open XML\u3002\u4F46\u662F\uFF0C\
  \u4E0EJSON\u76F8\u6BD4\uFF0C\u5B83\u8F83\u4E3A\u5197\u957F\uFF0C\u6F6E\u6D41\u5DF2\
  \u7ECF\u8F6C\u53D8\u3002JSON\u56E0\u5176\u8F7B\u91CF\u7EA7\u548C\u4E0EJavaScript\u7684\
  \u539F\u751F\u517C\u5BB9\u6027\u800C\u5728\u57FA\u4E8EWeb\u7684APIs\u4E2D\u83B7\u5F97\
  \u4E86\u5173\u6CE8."
title: "\u5904\u7406XML"
weight: 40
---

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
