---
title:                "C: 将字符串转换为大写"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写C程序时，有时候我们需要将字符串的首字母变成大写，这样可以提高程序的可读性和用户体验。例如，我们可能需要将用户输入的名字显示为大写首字母形式，或者格式化一些特定的数据。不管是什么原因，学习如何在C中实现字符串的首字母大写都是非常有用的技能。

## 如何做

在C语言中，首字母大写的方式有很多种，我们这里介绍一种简单的方法。首先，我们需要定义一个函数来实现字符串的首字母大写功能：

```C
void capitalize(char str[]){
    if(str[0]>=97&&str[0]<=122){ //检查首字母是否为小写字母
        str[0]-=32; //转换为大写字母
    }
}
```

上面的函数接受一个字符串作为参数，并且假设输入的字符串只包含字母和空格。首先，我们通过检查字符串的第一个字符是否在小写字母的ASCII码范围内来判断第一个字符是否为小写字母。然后，我们通过将第一个字符的ASCII码减去32来实现大写转换。最后，我们可以在主函数中使用这个函数来对字符串进行首字母大写操作：

```C
int main(){
    char name[] = "john";
    capitalize(name);
    printf("%s\n", name); //输出结果为"John"
    return 0;
}
```

## 深入了解

上面介绍的方法只是对字符串首字母大写的一种简单实现。实际上，字符串的首字母大写有很多种方式，并且实现的复杂程度也各不相同。一些更复杂的实现可能会考虑字符串中的数字和特殊字符，或者支持不同的语言。如果你对字符串的操作感兴趣，可以继续深入学习字符串的处理和转换技巧。

## 参考资料

- [How to capitalize a string in C](https://www.geeksforgeeks.org/c-program-captalize-first-letter-every-word-string/)
- [ASCII码表](http://www.asciitable.com/)
- [C字符串处理教程](http://www.runoob.com/cprogramming/c-strings.html)

## 参见

- [Markdown文档](https://www.markdownguide.org/)
- [C语言教程](http://www.runoob.com/cprogramming/c-tutorial.html)
- [字符串处理函数参考手册](https://www.tutorialspoint.com/c_standard_library/string_h.htm)