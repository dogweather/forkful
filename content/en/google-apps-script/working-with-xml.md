---
title:                "Working with XML"
date:                  2024-02-01T13:42:36.936019-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?

When we talk about working with XML in Google Apps Script, we’re dealing with the art of parsing, modifying, and generating XML data. Why bother? Because XML is still widely used for web services, configuration files, and more. It's a universal format for structured data that outlives its supposed demise.

## How to:

Let’s dive straight into how to handle XML with Google Apps Script. Suppose you have an XML string and you want to parse it, extract some information, and perhaps modify it.

### Parsing XML
```Google Apps Script
const xmlString = '<root><child name="foo">bar</child></root>';
const document = XmlService.parse(xmlString);
const root = document.getRootElement();
const child = root.getChild('child');
const nameAttribute = child.getAttribute('name').getValue();
console.log(nameAttribute);  // Output: foo
```

### Extracting Information
Let's extract data from an XML element.
```Google Apps Script
const childText = child.getText();
console.log(childText);  // Output: bar
```

### Modifying XML
Now, if you want to change some data:
```Google Apps Script
child.setText("newBar");
child.setAttribute(XmlService.createAttribute("name", "newFoo"));
const modifiedXmlString = XmlService.getPrettyFormat().format(document);
console.log(modifiedXmlString);
// Output:
// <root>
//   <child name="newFoo">newBar</child>
// </root>
```

### Generating XML
And here's how you might generate a simple XML document from scratch:
```Google Apps Script
const newDoc = XmlService.createDocument(XmlService.createElement("greetings")
  .addContent(XmlService.createElement("hello").setText("world")));
const xml = XmlService.getPrettyFormat().format(newDoc);
console.log(xml);
// Output:
// <greetings>
//   <hello>world</hello>
// </greetings>
```

## Deep Dive

XML has a rich history dating back to the late '90s, designed to carry data with both simplicity and complexity in mind. Google Apps Script utilizes Java's underpinning XML Service, offering a robust API for handling XML data. However, it's worth noting that in modern web development, JSON has largely taken over as the favored data interchange format due to its simplicity and natural compatibility with JavaScript. But, in scenarios where you're interfacing with legacy systems or specific SOAP-based web services, a solid grasp of XML manipulation in Google Apps Script can save the day.

For complex XML operations, Google Apps Script’s `XmlService` might feel cumbersome compared to modern JavaScript libraries or platforms built with native JSON support in mind. Yet, it remains a powerful tool, capable of handling intricate XML manipulations required in various business and administrative applications. Understanding when to leverage XML and Google Apps Script's capabilities therein, versus opting for JSON in web-based projects, depends on the project requirements and the ecosystems you're working within.
