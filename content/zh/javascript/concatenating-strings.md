---
title:    "Javascript: 合并字符串"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要将文本片段合并在一起。这可以通过使用Javascript中的字符串连接来实现。通过将两个或更多字符串连接在一起，我们可以创建一个全新的字符串，包含两个原始字符串的内容。这也是为什么我们对字符串连接感兴趣的原因。

## 如何

我们可以使用加号运算符来连接两个字符串。例如，我们想要将"I love"和"Javascript"这两个字符串连接起来，并且想要在它们之间添加一个空格，代码如下所示：

```Javascript
let firstString = "I love";
let secondString = "Javascript";
let finalString = firstString + " " + secondString;
console.log(finalString);
```

该代码的输出将会是"I love Javascript"。

如果我们想要在字符串中添加变量，我们可以使用模板字符串来实现。例如，我们想要在"I am learning"和"Javascript"之间插入一个变量来表示学习的阶段，代码如下所示：

```Javascript
let learningStage = "advanced";
let finalString = `I am learning ${learningStage} Javascript`;
console.log(finalString);
```

该代码的输出将会是"I am learning advanced Javascript"。

需要注意的是，当我们连接数字和字符串时，数字会被自动转换为字符串。例如，如果我们想要将数字2和字符串"Javascript"连接起来，代码如下所示：

```Javascript
let number = 2;
let finalString = number + " Javascript";
console.log(finalString);
```

该代码的输出将会是"2 Javascript"。

## 深入了解

在Javascript中，字符串连接是一项非常基础和常用的操作。除了使用加号运算符和模板字符串，我们还可以使用字符串连接方法：concat()来进行连接。

例如，我们想要连接"I love"、"learning"和"Javascript"这三个字符串，代码如下所示：

```Javascript
let firstString = "I love";
let secondString = "learning";
let thirdString = "Javascript";
let finalString = firstString.concat(" ", secondString, " ", thirdString);
console.log(finalString);
```

该代码的输出将会是"I love learning Javascript"。

值得注意的是，concat()方法不会改变原始的字符串，而是返回一个新的字符串。因此，我们需要将其赋值给一个新的变量，以便进一步使用。

## See Also

- [Javascript字符串方法](https://www.w3schools.com/js/js_string_methods.asp)
- [模板字符串](https://codeburst.io/javascript-es6-template-literals-and-tag-functions-a2ce322915ab)
- [字符串连接简介](https://www.howtogeek.com/637143/how-to-combine-strings-in-javascript/)

*Markdown文件结束。*