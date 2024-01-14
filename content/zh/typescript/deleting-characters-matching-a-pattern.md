---
title:                "TypeScript: 删除匹配模式的字符"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

当我们编写程序时，有时候需要从字符串中删除特定的字符。这可能是因为我们需要对字符串进行格式化，或者只是想要提取一个子字符串。无论是什么原因，TypeScript都提供了一种简单且高效的方法来删除匹配特定模式的字符。

## 怎么做

首先，我们需要使用字符串的 `replace()` 方法来替换我们想要删除的字符。这个方法接受两个参数，第一个是我们想要删除的模式，可以是一个字符串或者一个正则表达式。第二个参数是替换后的字符，这里我们可以传入空字符串来实现删除的效果。

例如，我们有一个字符串 `Hello World!`，现在我们想要删除所有的元音字母，我们可以这样做：

```TypeScript
const str = 'Hello World!';
const newStr = str.replace(/[aeiou]/gi, ''); // g 表示全局匹配，i 表示忽略大小写

console.log(newStr); // 输出: Hll Wrld!
```

在上面的例子中，我们使用了一个正则表达式 `/[aeiou]/gi` 来匹配所有的元音字母，并将它们替换为空字符串，从而实现了元音字母的删除。

## 深入探讨

除了简单的替换操作，我们还可以通过使用回调函数来实现更加复杂的删除操作。回调函数会在每次匹配成功后调用，我们可以在回调函数中进行一些逻辑处理后返回我们想要替换的字符串。

例如，我们想要将所有的数字乘以2并删除小数点后的数字，我们可以这样做：

```TypeScript
const str = 'My favorite numbers are 2, 6.5 and 10!';
const newStr = str.replace(/\d+(\.\d+)?/g, (match, p1) => {
  const num = parseFloat(match);
  return (num * 2).toFixed(p1 ? p1.length : 0);
});

console.log(newStr); // 输出: My favorite numbers are 4, 13.00 and 20!
```

在这个例子中，我们使用了一个正则表达式 `/d+(\.d+)?/g` 来匹配数字，然后在回调函数中使用 `parseFloat()` 方法将字符串转换为数字，并使用 `toFixed()` 方法来控制小数点后的位数。

## 参考链接

1. [MDN - String.prototype.replace()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [MDN - RegExp](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
3. [TypeScript - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)

---

## 参考链接

1. [MDN - String.prototype.replace()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [MDN - RegExp](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
3. [TypeScript - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)