---
title:    "C++: 检查目录是否存在"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 为什么

在进行文件和文件夹操作时，检查文件夹是否存在是一个非常重要的步骤。这可以帮助我们避免错误，比如创建重复的文件夹，或者在一个不存在的文件夹中尝试创建文件。通过编写一个简单的程序来检查文件夹是否存在，我们可以更高效地管理我们的文件系统。

# 如何操作

```C++
#include <iostream>
#include <filesystem> //包含文件和文件夹操作函数的标准库

using namespace std;
namespace fs = std::filesystem;

int main() {
  // 定义文件夹路径
  std::string folderPath = "/Users/username/Documents";

  // 使用exists函数检查文件夹是否存在
  if (fs::exists(folderPath)) {
    std::cout << "文件夹已存在" << std::endl;
  } else {
    std::cout << "文件夹不存在" << std::endl;
  }

  return 0;
}
```

**输出：**

文件夹已存在

在上面的代码中，我们使用了C++标准库中的`<filesystem>`头文件来调用`exists`函数来检查指定的文件夹路径是否存在。如果文件夹存在，`exists`函数将返回`true`，否则将返回`false`。

如果我们要创建一个新的文件夹，我们可以使用`create_directory`函数来实现，但是在创建之前，我们仍然需要通过`exists`函数来检查文件夹是否已存在。

# 深入探索

当我们调用`exists`函数时，它会返回一个布尔值。但是，如果我们想要更多的信息，比如文件夹的更多属性，我们可以使用`status`函数来代替`exits`函数。这样我们将得到一个`std::file_status`对象，它包含了有关文件夹的更多信息，比如文件夹的大小和最后修改时间等。

另外，如果我们只需要检查文件夹是否存在，我们可以使用`is_directory`函数来代替`exists`函数。这样我们可以更加精确地判断给定路径是否是一个文件夹。

# 见其他

- [`<filesystem>`标准库文档](https://en.cppreference.com/w/cpp/filesystem)
- [C++中检查文件夹是否存在的其他方法](https://stackoverflow.com/questions/5923330/how-to-check-if-a-directory-exists-in-a-c-program)

# 参考链接
-  [C++ reference](https://zh.cppreference.com/) (中文)
- [C++标准库（英文）](https://en.cppreference.com/w/)