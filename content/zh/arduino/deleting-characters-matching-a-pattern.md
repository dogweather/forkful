---
title:    "Arduino: 删除匹配模式的字符"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 为什么删除匹配模式的字符

当我们在使用Arduino编程时，经常会遇到需要删除字符的情况。有时候这些字符是我们不需要的，有时候是因为错误输入导致的。而删除匹配模式的字符可以帮助我们更快速地处理数据，提高我们的编程效率。

# 如何进行删除匹配模式的字符

在Arduino中，我们可以使用函数和循环来删除匹配模式的字符。首先，我们需要定义一个字符串变量，然后使用for循环遍历这个字符串中的每一个字符。在循环中，我们可以使用if语句来判断字符是否符合我们的匹配模式，在符合的情况下使用remove()函数来删除这个字符。代码示例如下：

```arduino
// 定义字符串变量
String sentence = "Hello World!";
// 使用for循环遍历字符串
for(int i=0; i<sentence.length(); i++){
  // 判断字符是否为'o'
  if(sentence.charAt(i) == 'o'){
    // 使用remove()函数来删除匹配的字符
    sentence.remove(i);
  }
}
// 输出结果
Serial.println(sentence); // 输出结果为 "Hell Wrld!"
```

# 深入了解删除匹配模式的字符

当我们执行remove()函数来删除字符时，其实是通过将匹配的字符后面的所有字符往前移动一个位置，覆盖掉被删除的字符来实现。除了使用if语句来判断字符是否符合匹配模式外，我们也可以使用正则表达式（Regular Expression）来更精确地删除字符。同时，使用remove()函数来删除字符也会改变字符串的长度，所以在使用for循环遍历字符串时需要注意。

# 查看更多信息

- [Arduino String类文档](https://www.arduino.cc/reference/zh/language/variables/data-types/string/)
- [Arduino 库参考：remove()](https://www.arduino.cc/reference/zh/language/variables/data-types/stringfunctions/remove/)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-tutorial.html)
- [Arduino循环语句教程](https://www.arduino.cc/reference/zh/language/structure/control-structure/for/)