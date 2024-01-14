---
title:                "C++: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 작업에 참여하는 이유는 여러 가지가 있습니다. 문자열이 얼마나 긴지 알면, 가변 길이 배열을 사용할 수 있고, 메모리 할당을 최적화하고, 출력 형식을 결정할 수 있기 때문입니다.

## 어떻게

```C++ 
#include <iostream> 
#include <cstring> 
  
using namespace std; 
  
int main() 
{ 
    char str[] = "안녕하세요!"; 
    
    // C++ 함수를 이용한 문자열 길이 찾기
    int length = strlen(str); 
    
    // 출력 및 확인
    cout << "문자열의 길이는 " << length << "입니다." << endl; 
    return 0; 
} 
```

```C++ 
#include <iostream> 
#include <string> 
  
using namespace std; 
  
int main() 
{ 
    string str = "안녕하세요!"; 
    
    // C++ 클래스 메서드를 이용한 문자열 길이 찾기
    int length = str.length(); 
    
    // 출력 및 확인
    cout << "문자열의 길이는 " << length << "입니다." << endl; 
    return 0; 
} 
```

### 딥 다이브

문자열의 길이를 찾는 방법은 간단합니다. 사용 가능한 문자열 길이를 저장하는 변수를 만들고, 널 ('\0') 문자를 만날 때까지 문자열을 반복해서 이동하면 됩니다. C++에서는 strlen()과 string::length() 함수를 사용하여 이 작업을 수행할 수 있습니다.

그러나 이러한 함수는 문자열 끝이 널 문자로 끝나기 때문에, 문자열에 널이 아니지만 필요한 길이를 찾아야 하는 경우가 있을 수 있습니다. 이 경우, 반복문과 문자열의 인덱스를 사용하여 수동으로 문자열의 길이를 계산해야 합니다.

또한, 오늘날 대부분의 표준 라이브러리에서는 문자열의 길이를 쉽게 찾을 수 있는 함수를 제공하기 때문에, 굳이 수동으로 계산할 필요가 없습니다. 따라서, 복잡한 코드를 작성할 필요 없이도 문자열의 길이를 쉽게 찾을 수 있습니다.

## 참조

- [C++ string 클래스](https://www.cplusplus.com/reference/string/string/)
- [C++ string 클래스를 사용하여 문자열 길이 찾기](https://www.geeksforgeeks.org/length-of-a-string-using-string-class-in-c/)
- [C++ strlen() 함수](https://www.cplusplus.com/reference/cstring/strlen/) 
- [문자열의 길이 찾기 (C++)](https://modoocode.com/95)