---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "C++: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 원하는 패턴에 일치하는 문자를 삭제하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 이 작업은 보통 문자열에서 불필요한 데이터를 제거하거나 원하는 형식으로 데이터를 정리하기 위해 필요합니다.

## 방법:
다음은 문자열에서 포함된 "a" 문자를 제거하는 간단한 예제입니다.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 예제 문자열
    string str = "apple, banana, carrot";
    
    // "a" 문자 제거
    str.erase(remove(str.begin(), str.end(), 'a'), str.end());

    // 결과 출력
    cout << str; // "pple, bn, crot"
    
    return 0;
}
```

위 코드에서는 <code>remove</code> 함수를 사용하여 문자열에서 해당하는 문자를 모두 제거하고, <code>erase</code> 함수를 사용하여 삭제된 문자열을 재할당합니다. 이 방법을 사용하면 원하는 패턴에 해당하는 문자를 간편하게 삭제할 수 있습니다.

## 깊이 파고들기:
### 역사적 배경:
패턴에 일치하는 문자를 삭제하는 방법은 오래전부터 프로그래밍에서 사용되어왔습니다. 그러나 과거에는 이러한 작업을 수행하는 데에는 더 많은 수작업이 필요했습니다. 그래서 현재처럼 코드 한 줄만으로 작업을 완료할 수 있던 것은 컴퓨터 과학 기술의 발전 덕분입니다.

### 대안:
패턴에 일치하는 문자를 삭제하는 대안으로는 정규표현식이 있습니다. 정규표현식은 문자열에서 원하는 패턴을 찾고 삭제하는 데에도 사용할 수 있습니다. 그러나 정규표현식을 사용하면 코드의 이해가 어려워지고, 일부 언어에서는 성능도 저하될 수 있습니다.

### 구현 세부사항:
위에서 사용한 코드에서 <code>erase</code> 함수의 내부 구현은 다음과 같은 과정을 거칩니다.
1. 문자열에서 패턴에 일치하는 문자를 모두 제거한 후 제거된 문자열의 새로운 끝 위치를 반환합니다.
2. 반환된 위치부터 문자열의 끝까지의 문자를 모두 제거합니다.
3. 제거할 문자가 없다면 문자열의 끝 위치를 그대로 반환합니다.

## 참고:
- <a href="https://www.geeksforgeeks.org/delete-characters-matching-pattern-string/">GeeksforGeeks - Delete characters matching a pattern in a string</a>
- <a href="https://www.geeksforgeeks.org/remove-characters-from-the-first-string-which-are-present-in-the-second-string/">GeeksforGeeks - Remove characters from the first string which are present in the second string</a>
- <a href="https://www.regular-expressions.info/">Regular-Expressions.info - Regular Expressions Tutorial</a>