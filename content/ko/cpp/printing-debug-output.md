---
title:                "디버그 출력 프린팅"
html_title:           "C++: 디버그 출력 프린팅"
simple_title:         "디버그 출력 프린팅"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버그 출력(printing debug output)이란 무엇인지 알고 싶으세요? 그것은 우리가 우리가 작성한 코드가 정확하게 실행되고 디버그 과정에서 발생하는 문제를 추적하는 데 도움을 줍니다. 따라서 프로그래머들은 코드를 디버그하는 과정에서 이를 사용합니다.

## 어떻게:
디버그 출력을 수행하는 것은 매우 간단합니다. 첫째, 우리는 ```C++ ... ``` 코드 블록에서 우리의 코드를 작성합니다. 그런 다음, 디버그 출력을 원하는 부분에 ```std::cout``` 함수를 사용하여 해당 부분의 값을 출력합니다. 아래 예시를 참조하세요.

```C++
int num1 = 5;
int num2 = 10;
int sum = num1 + num2;

std::cout << "Sum of " << num1 << " and " << num2 << " is: " << sum << std::endl;
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
Sum of 5 and 10 is: 15
```

## 깊이 파고들기:
디버그 출력은 프로그래밍에서 매우 중요한 역할을 합니다. 디버그 출력을 사용하여 우리의 코드를 쉽게 디버그하고 문제를 해결할 수 있습니다. 이 기술은 예전부터 사용되어 왔으며, 오늘날에도 여전히 광범위하게 사용되고 있습니다. 그러나 디버그 출력은 코드 실행 시간을 느리게 할 수 있으므로 대안과 함께 사용할 때 주의해야 합니다. 또한 해당 값을 수동으로 출력하는 대신 디버거와 같은 도구를 사용하여 디버그 출력을 관리할 수 있습니다. 이는 코드를 깔끔하게 유지하는 데 도움이 됩니다.

## 참고 자료:
디버그 출력에 대해 더 알아보고 싶으시다면 아래 링크를 참고하세요.

- [C++ Reference - std::cout](http://www.cplusplus.com/reference/iostream/cout/)
- [GeeksforGeeks - Debugging Techniques in C/C++](https://www.geeksforgeeks.org/debugging-techniques-in-c-c/)