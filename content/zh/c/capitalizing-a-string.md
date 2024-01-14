---
title:    "C: 将字符串大写化"
keywords: ["C"]
---

{{< edit_this_page >}}

## 为什么

有时候在编写程序时，我们需要将字符串中的每个单词的首字母大写。这可能是为了美观、可读性或特定的文本格式要求。无论是什么原因，学会如何将字符串中的字母转换为大写是非常有用的技能。

## 如何做

首先，让我们看一下下面这个简单的C代码示例，它会接收一个字符串作为输入，并将其每个单词的首字母大写。让我们来看看运行后的结果是什么：

```C
#include <stdio.h>
#include <string.h>

int main() {

    char str[50];

    printf("请输入一个字符串: ");
    fgets(str, 50, stdin);

    for (int i = 0; i < strlen(str); i++) {
        //检查字符串中的空格
        if (str[i] == ' ') {
            i++;
            //将下一个字符的ASCII值转换为大写字母
            str[i] = toupper(str[i]);
        }
        else {
            //将当前字符的ASCII值转换为小写字母
            str[i] = tolower(str[i]);
        }
    }

    //打印结果
    printf("转换后的字符串是: %s", str);

    return 0;
}
```

输入：hello world
输出：Hello World

让我们来分析一下这段代码。首先，我们使用`fgets()`函数从用户那里接收一个字符串，并将其存储在名为`str`的字符数组中。然后，使用`for`循环遍历字符串中的每个字符。在循环中，我们使用`tolower()`和`toupper()`函数来将字符的ASCII值转换为小写或大写。在遇到空格后，我们将下一个字符转换为大写，从而达到每个单词首字母大写的效果。最后，我们使用`printf()`函数将结果打印出来。

请注意，在上面的代码示例中，我们使用了`<string.h>`头文件中的`strlen()`函数来获取字符串的长度。如果您不是很熟悉C语言，请务必阅读这个函数的文档以深入了解它的用法。同样地，如果您对上面提到的任何其他函数不熟悉，请务必查阅它们的文档。

## 深入了解

如果您对字符串转换更深层次的原理感兴趣，可以了解一下ASCII码。ASCII是一种用于表示文本字符串的编码标准，它将每个字符映射到一个唯一的数字。`tolower()`和`toupper()`函数实际上就是通过在ASCII表中添加或减去一个固定值来转换大小写。

另外，您也可以尝试使用其他方法来实现字符串的首字母大写转换，比如使用指针和`strtok()`函数。通过挑战自己，您可以加深对C语言编程的理解和掌握。

## 参考链接

- [C语言字符串函数文档](http://www.cplusplus.com/reference/cstring/)
- [ASCII码表](https://baike.baidu.com/item/ASCII/309296?fr=aladdin)
- [使用指针和strtok()实现字符串首字母大写转换的代码示例](https://www.tutorialspoint.com/c-program-to-capitalize-first-letter-of-each-word-in-string)
- [更多关于C语言的学习资源](https://www.cprogramming.com/tutorial/c-tutorial.html)

## 参见

- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C语言基础入门](https://www.runoob.com/cprogramming/c-tutorial.html)