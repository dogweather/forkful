---
title:                "C: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#为什么

许多时候，在C编程中，我们需要检查一个目录是否存在。这通常是因为我们需要在程序中使用该目录，但不确定它是否存在。因此，学习如何检查目录是否存在是很有用的，可以帮助我们在程序中正确地处理这种情况。

#如何编码

为了检查目录是否存在，我们可以使用C语言中的“access”函数。该函数接受两个参数，第一个是目录的路径，第二个是要执行的操作。例如，我们可以使用“F_OK”来检查目录的存在性，如下所示：

```C
#include <stdio.h> 
#include <stdlib.h> 
#include <unistd.h> 

int main() 
{ 
	if (access("/Users/MyDirectory", F_OK) != -1) 
		printf("目录存在\n"); 
	else
		printf("目录不存在\n"); 
	 
	return 0; 
}
```

在上面的代码中，我们使用“access”函数来检查路径为“/Users/MyDirectory”的目录是否存在。如果存在，代码将打印“目录存在”，否则打印“目录不存在”。

#深入了解

“access”函数提供了很多不同的操作选项，例如“R_OK”用于检查目录是否可读，而“W_OK”用于检查目录是否可写。我们还可以使用“X_OK”来检查目录是否可执行。通过组合这些选项，我们可以针对不同的需求来检查目录的存在性。

此外，我们还可以使用“stat”函数来检查目录是否存在。使用该函数，我们可以获取目录的详细信息，并检查其是否存在。但是，相比较“access”函数，使用“stat”函数稍微麻烦一些。

#另请参阅

- [C语言教程](https://www.w3schools.in/c-tutorial/)
- [使用C语言创建目录](https://www.geeksforgeeks.org/create-directoryfolder-cc-program/)
- [C语言的文件操作](https://www.programiz.com/c-programming/c-file-input-output)