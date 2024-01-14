---
title:                "TypeScript: 转换字符串为小写"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要转换字符串为小写？

字符串是编程中经常使用的一项基本数据类型。转换字符串为小写可以帮助我们实现一些特定的功能，比如在搜索和排序时更容易比较字符串，以及在规范化用户输入时。在TypeScript中，我们可以使用内置函数toLowerCase()来实现这一功能。

## 如何转换字符串为小写？

```TypeScript
let str = "HELLO WORLD";
console.log(str.toLowerCase());
```

该代码将输出 "hello world"，因为所有的字母都被转换为小写。

我们也可以使用循环来转换一个字符串中的每个字母：

```TypeScript
let str = "H3110 W0R1D";
let result = "";

for (let i = 0; i < str.length; i++) {
  let char = str[i];
  if (char >= "A" && char <= "Z") {
    result += char.toLowerCase();
  } else {
    result += char;
  }
}

console.log(result);
```

该代码将输出 "h3110 w0r1d"，因为它遍历字符串中的每个字符，如果是大写字母，则转换为小写并添加到新的字符串中。否则将保留原字符。

## 深入了解转换字符串为小写

在JavaScript中，字符串是不可变的，这也意味着转换为小写的字符串本身并不会更改。相反，toLowerCase()函数会返回一个新的字符串，原始字符串保持不变。

除了toLowerCase()，我们还可以使用toLocaleLowerCase()函数来将字符串转换为小写，它可以根据当前地区的规则来转换字符串。

另外，我们也可以使用正则表达式来匹配和替换字符串中的大写字符：

```TypeScript
let str = "Hello TypeScript";
let newStr = str.replace(/[A-Z]/g, (match) => {
  return match.toLowerCase();
});

console.log(newStr);
```

该代码将输出 "hello typeScript"，因为它使用正则表达式来匹配所有大写字母，并替换为对应的小写字母。

## 参考链接

- [TypeScript官方文档：String](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- [MDN文档：String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN文档：String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [MDN文档：Regular Expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)