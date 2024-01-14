---
title:    "TypeScript: 字符串大写化"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 为什么

大家好！作为一个程序员，我们经常需要对字符串进行各种操作。其中一个常见的操作就是给字符串首字母大写。这样做的原因可能有很多，比如想要满足特定的命名规范，或者让字符串看起来更规整一些。在本篇文章中，我们会为大家介绍如何用TypeScript来给字符串首字母大写，让你的代码更加规范有序。

## 如何

成功销售，家族企业，头脑风暴，以上几个词组都是由多个单词组成的。但是，在编程时，这样的词组并不能满足命名规范。为了让这些词组看起来更加美观，我们可以用TypeScript来给它们的首字母大写。

首先，我们需要定义一个字符串变量，比如`phrase`。

```TypeScript
let phrase: string = "成功销售，家族企业，头脑风暴";
```

然后，我们可以用`charAt()`方法来获取字符串的第一个字符，并用`toUpperCase()`方法来将它转换成大写。

```TypeScript
let firstChar: string = phrase.charAt(0).toUpperCase(); // "C"
```

接着，我们可以使用字符串的`slice()`方法来获取剩余的字符，并用`toLowerCase()`方法将它们转换成小写。

```TypeScript
let restOfString: string = phrase.slice(1).toLowerCase(); // "heng gong xiao shou，jia zu qi ye，tou nao feng bao"
```

最后，我们只需要将第一个字符和剩余字符连接起来，就可以得到首字母大写的字符串了！

```TypeScript
let newPhrase: string = firstChar + restOfString; // "成功销售，家族企业，头脑风暴"
```

让我们来看一下完整的代码和输出结果：

```TypeScript
let phrase: string = "成功销售，家族企业，头脑风暴";
let firstChar: string = phrase.charAt(0).toUpperCase();
let restOfString: string = phrase.slice(1).toLowerCase();
let newPhrase: string = firstChar + restOfString;

console.log(newPhrase); // "成功销售，家族企业，头脑风暴"
```

## 深入了解

除了上述介绍的方法，我们还可以用`split()`方法来根据特定的分隔符来将字符串拆分成数组，然后对每个单词进行首字母大写的操作。最后再用`join()`方法来将数组拼接成字符串。

```TypeScript
let phrase: string = "成功，销售，家族，企业，头脑，风暴";
let words: Array<string> = phrase.split("，"); // ["成功", "销售", "家族", "企业", "头脑", "风暴"]
let capitalizedWords: Array<string> = words.map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase()); // ["成功", "销售", "家族", "企业", "头脑", "风暴"]
let newPhrase: string = capitalizedWords.join("，"); // "成功，销售，家族，企业，头脑，风暴"

console.log(newPhrase); // "成功，销售，家族，企业，头脑，风暴"
```

不仅如此，我们还可以用正则表达式来实现首字母大写的操作。正则表达式是一种强大的文本匹配工具，可以用来识别符合特定规则的文本。

```TypeScript
let phrase: string = "成功销售，家族企业，头脑风暴";
let newPhrase: string = phrase.replace(/\b\w/g, (char) => char.toUpperCase()); // "成功销售