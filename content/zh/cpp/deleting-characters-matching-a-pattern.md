---
title:    "C++: 删除匹配模式的字符"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么要删除匹配模式的字符

在编程中，有时会遇到需要删除字符的情况。而有时，我们可能不想一个一个手动删除，而是想要用一种更高效的方式来删除特定的字符。这时候，我们可以使用删除匹配模式的字符的方法来完成这个任务。

## 如何操作

下面是一个使用C++来删除匹配模式字符的例子： 

```C++
#include <bits/stdc++.h> 
#define MAX 100 

using namespace std; 

// 函数来删除给定的字符 
int deleteCharacters(char str[], char to_be_deleted) 
{ 
	int count = 0; 

	// 遍历整个数组
	for (int i = 0; str[i]; i++) { 

		// 如果遇到匹配的字符，就跳过
		if (str[i] == to_be_deleted) 
			continue; 

		// 否则，将字符复制到新的数组中
		str[count++] = str[i]; 
	} 
	str[count] = '\0'; 

	return count; 
} 

int main() 
{ 
	// 例子输入
	char str[] = "Hello World!"; 
	char to_be_deleted = 'l'; 
	
	// 调用函数来删除给定的字符
	int num = deleteCharacters(str, to_be_deleted); 

	// 输出结果
	cout << "String after deleting " << num << " " << to_be_deleted << "'s: " << str; 

	return 0; 
} 
```

输出结果应该为：

```
String after deleting 2 l's: Heo Word!
```

## 深入了解

当我们删除匹配模式的字符时，需要考虑一些边界情况和特殊情况。例如，如果字符串中有多个匹配字符，我们应该删除所有匹配的字符，而不仅仅是第一个。另外，我们也需要注意被删除字符后面的字符是否需要移动，以保持字符串的连续性。

此外，如果我们想要删除的字符不仅仅是一个字符，而是一个字符串，我们也可以使用类似的方法来删除。

## 查看更多

- [C++字符串函数参考](https://www.runoob.com/cplusplus/cpp-standard-library-string-functions.html)
- [C++字符串与字符数组相关操作](https://blog.csdn.net/fengtao5557/article/details/79814002)
- [C++指针与数组的深入理解](https://www.cnblogs.com/mirage003/p/9956716.html)

## 参考来源

此文章主要参考自 [GeeksforGeeks](https://www.geeksforgeeks.org/remove-all-occurrences-of-a-character-in-a-string/)