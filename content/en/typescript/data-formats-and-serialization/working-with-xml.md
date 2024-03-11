---
date: 2024-01-25 03:39:28.770982-07:00
description: "Working with XML means parsing, manipulating, and writing XML data using\
  \ programming. Programmers handle XML to exchange data across different systems,\u2026"
lastmod: '2024-03-11T00:14:33.740139-06:00'
model: gpt-4-1106-preview
summary: "Working with XML means parsing, manipulating, and writing XML data using\
  \ programming. Programmers handle XML to exchange data across different systems,\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML means parsing, manipulating, and writing XML data using programming. Programmers handle XML to exchange data across different systems, for configuration files, or when working with standards like SOAP that rely on XML.

## How to:
```TypeScript
import { parseString } from 'xml2js';

// Sample XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Don't forget the meeting!</body>
             </note>`;

// Parse XML to JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Assuming parse was successful, output might look like:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Don't forget the meeting!'] } 
}
```

## Deep Dive
XML, or Extensible Markup Language, has been around since the late '90s. Its self-descriptive nature and human-readable format made it a hit early on for various applications such as RSS feeds, configuration management, and even office document formats like Microsoft Office Open XML. But, it's verbose compared to JSON, and the tide's been turning. JSON's gotten the spotlight for web-based APIs due to its lighter weight and native JavaScript compatibility.

Nonetheless, XML's not dead. It’s used in large-scale enterprise systems and for document standards that haven't shifted to JSON. Tools like `xml2js` for TypeScript or `lxml` in Python prove there's a continued need for XML manipulation in programming.

TypeScript doesn't have built-in support for XML like it does for JSON. Instead, you work with libraries. `xml2js` is an example. It transforms XML into JSON, making the data easier for JavaScript gurus to work with.

## See Also
- [MDN Web Docs on XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm package](https://www.npmjs.com/package/xml2js)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)
