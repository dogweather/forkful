---
title:    "C++: 문자열 연결하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

# 왜
문자열을 연결(concatenate) 하는 것이 중요한 이유는 여러 가지가 있지만, 가장 큰 이유는 데이터를 보다 간편하게 조작하고 사용하기 위해서입니다. 문자열을 연결함으로써, 다양한 기능들을 사용할 수 있으며, 보다 쉽고 빠르게 프로그래밍할 수 있습니다.

# 어떻게
```c++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string first = "Hello";
  string second = "World";
  string result = first + " " + second;
  
  cout << result << endl;
  
  return 0;
}
```

위의 예제는 두 개의 문자열을 + 연산자를 사용하여 연결하는 간단한 방법을 보여줍니다. "Hello"와 "World"라는 두 문자열이 "Hello World"라는 하나의 문자열로 연결되어 출력됩니다. 이처럼, 여러 개의 문자열을 연결함으로써 보다 복잡한 출력을 만들 수 있습니다.

# 자세히 살펴보기
문자열을 연결하는 과정은 보통 문자열의 끝에 다른 문자열을 추가하는 것으로 이루어집니다. 이때, 두 문자열을 연결하기 위해 연산자를 사용하면, 두 문자열을 복사하여 새로운 문자열을 만들게 됩니다. 따라서, 여러 번의 연결 작업이 필요하다면, 시간과 메모리 등의 리소스를 더 많이 사용하게 됩니다. 또한, 문자열을 연결할 때, 문자열의 길이를 미리 알고 있어야 합니다.

더욱 효율적인 방법은 문자열을 나타내는 클래스를 이용하는 것입니다. C++에서는 기본적으로 문자열을 처리하기 위해 string 클래스를 제공합니다. 이 클래스는 문자열의 길이를 동적으로 관리하며, + 연산자를 오버로딩하여 문자열을 연결할 때 추가적인 복사를 최소화합니다. 또한, 문자열의 길이를 미리 알 필요가 없으므로 보다 유연한 프로그래밍이 가능합니다.

# 관련 자료
[문자열 연결 설명서](https://docs.microsoft.com/ko-kr/cpp/standard-library/basic-string-functions?view=msvc-160 "문자열 연결 설명서")

[문자열 처리를 위한 string 클래스 사용법](https://www.learncpp.com/cpp-tutorial/97-overloading-the-operator-for-member-functions/ "string 클래스 사용법")