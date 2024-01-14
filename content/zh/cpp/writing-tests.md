---
title:    "C++: 编写测试"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么编写测试?

编写测试是软件开发过程中的重要组成部分。测试可以帮助我们发现代码中的错误，确保程序可以按照预期的方式运行。它可以提高代码的质量和可靠性，从而减少在开发过程中可能出现的问题。

## 如何编写测试

```C++
#include <iostream>
#include <cassert>

using namespace std;

// A function that reverses a string 
string reverseString(string s) {
    string reversed = "";
    for(int i = s.length()-1; i >= 0; i--){
        reversed += s[i];
    }
    return reversed;
}

int main() {
    // Test case 1: single letter 
    string s = "a";
    assert(reverseString(s) == "a");
    
    // Test case 2: word with even number of letters
    s = "hello";
    assert(reverseString(s) == "olleh");

    // Test case 3: word with odd number of letters
    s = "software";
    assert(reverseString(s) == "erawtfos");

    // All tests passed 
    cout << "All tests passed. Great job!" << endl;

    return 0;
}
```

输出：

```
All tests passed. Great job!
```

编写测试的第一步是确定实现功能的代码。在这个例子中，我们有一个函数 `reverseString`，它接受一个字符串作为参数，并返回反转后的字符串。我们使用 `assert` 语句来验证函数的输出是否与我们期望的一致。

为了编写全面的测试，我们可以模拟不同的输入情况，并验证函数的输出是否正确。在上面的代码示例中，我们测试了单个字母、偶数字母和奇数字母的情况。如果测试通过，我们会得到一个成功的输出，说明函数的逻辑是正确的。

## 深入学习编写测试

除了上面提到的基本知识外，编写测试还有许多其他方面需要深入学习。例如，测试覆盖率是评估测试的有效性的重要指标，它可以帮助我们确定测试是否覆盖了代码的所有部分。另外，选择适当的测试框架也是很重要的，它可以简化测试的编写，并提供更多的功能。

编写测试还可以帮助我们提高代码的可读性和可维护性。通过编写测试，我们可以更清晰地了解代码的功能和逻辑，从而在需要更改代码时更容易做出正确的修改。

## 查看更多

- [测试驱动开发（TDD）指南](https://zhuanlan.zhihu.com/p/69094325)
- [Google Test: 一个C++测试框架](https://github.com/google/googletest)
- [测试覆盖率和代码覆盖度的简单解释](https://zhuanlan.zhihu.com/p/170856439)