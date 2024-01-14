---
title:    "C++: 정규식 사용하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜?

정규 표현식을 사용하는 이유는 무엇인가요? 모든 프로그래머에게 잘 알려진 것은 아니기 때문에 불필요한 시간 낭비를 피하기 위해서라면 다음을 읽어보세요.

## 어떻게?

정규 표현식을 어떻게 사용할까요? 일반적인 C++ 코드를 사용하여 예제와 출력을 살펴보고 실제로 어떻게 동작하는지 알아보세요.

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
  // 정규 표현식을 사용해 hello를 찾는 예제
  string str = "hello world";
  smatch match;
  regex reg("hello");
  if (regex_search(str, match, reg)) {
    cout << match.str() << endl; // 출력 결과: hello
  }
}
```

## 깊이 들어가기

더 나은 프로그래머가 되기 위해 더 많은 정보를 알고 싶나요? 정규 표현식을 사용하여 문자열을 검색, 대체 및 매칭하는 다양한 방법과 적절한 문법을 익힐 수 있습니다.

참고: [C++ 정규 표현식 참조 가이드](https://www.cplusplus.com/reference/regex/)

# 또 보기

- [C++ 문자열 처리 가이드](https://www.cplusplus.com/reference/string/)
- [정규 표현식 사용 예제 (한글)](https://velog.io/@max9106/%EC%A0%95%EA%B7%9C%ED%91%9C%ED%98%84%EC%8B%9D%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%EB%AC%B8%EC%9E%90%EC%97%B4-%EA%B2%80%EC%83%89%ED%95%98%EA%B8%B0)