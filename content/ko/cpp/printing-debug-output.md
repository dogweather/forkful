---
title:    "C++: 디버그 출력 프린팅"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하면서 디버그 출력을 하는 이유는 무엇일까요? 디버그 출력을 통해 코드의 문제점을 파악하고 수정하는데 도움을 줄 수 있습니다.

## 어떻게

디버그 출력을 하는 방법은 간단합니다. ```C++ cout```나 ```printf``` 함수를 사용해서 출력하면 됩니다. 다음은 코드 예시와 출력 예제입니다.

```C++
#include <iostream>
using namespace std;

int main() {
    int a = 5;
    int b = 7;
    cout << "a의 값은 " << a << "입니다." << endl;
    cout << "b의 값은 " << b << "입니다." << endl;
    int c = a + b;
    cout << "a와 b의 합은 " << c << "입니다." << endl;
    return 0;
}
```

출력 결과는 다음과 같습니다.

```
a의 값은 5입니다.
b의 값은 7입니다.
a와 b의 합은 12입니다.
```

간단한 코드에서는 디버그 출력이 크게 필요하지 않지만, 복잡한 코드에서는 디버그 출력을 통해 변수의 값을 확인하고 코드의 흐름을 따라가는게 더욱 쉬울 수 있습니다.

## 딥 다이브

디버그 출력에 대해 더 깊이 알아보겠습니다. 디버그 출력은 여러 가지 형태로 사용할 수 있습니다. 예를 들어 ```cout``` 함수를 사용할 때 ```endl``` 대신 ```\n```을 사용해도 같은 결과가 나옵니다. 또한 변수의 값을 출력할 때, ```+``` 연산자 대신 ```<<```를 사용해도 되지만, 여러 개의 변수를 한 번에 출력할 때는 ```+``` 연산자를 사용하는 것이 좀 더 편할 수 있습니다.

그리고 디버그 출력은 코드를 실행하면서 변수의 값이 변경되는지 확인하는데에도 유용합니다. 특히 반복문에서 변수의 값을 출력하면서 코드를 따라가는 것은 디버깅을 할 때 큰 도움이 될 수 있습니다.

## 더 자세한 정보

- [C++ 언어 사전: 디버그 출력](http://www.yes24.com/Product/Goods/15049117)
- [C++ 문법 총정리 - 디버깅 및 에러 핸들링](https://boycoding.tistory.com/31)
- [디버그 출력과 디버깅의 차이](http://blog.daum.net/swsky100/54)

## 관련 링크

- [Markdown 사용법](https://heropy.blog/2017/09/30/markdown/)
- [C++ 언어 사전](http://www.yes24.com/24/category/search?keywordAd=&keyword=&domain=BOOK&qdomain=%B1%BB%B8%AE%B5%B5#List_Title)