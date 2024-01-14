---
title:    "C++: 텍스트 검색 및 대체"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜?

텍스트를 검색하고 바꾸는 것에 참여하는 이유는 무엇일까요? 그것은 매우 유용한 도구입니다. 그것은 코드 작성을 효율적으로 만들고, 시간과 노력을 절약해줍니다. 특히 큰 프로젝트에서는 많은 양의 코드를 검색하고 바꾸는 것이 필수적이기 때문에 이 작업을 자동화하는 것은 더 중요합니다.

## 방법

검색과 바꾸기는 다양한 프로그래밍 언어에서 지원되는 기능입니다. 여기서는 C++을 예로 들겠습니다.

먼저, `string`이라는 클래스를 사용하여 텍스트를 저장해야 합니다. 예를 들어, "Hello, world!"라는 문자열을 저장하려면 다음과 같이 할 수 있습니다.

```C++
string text = "Hello, world!";
```

그런 다음 `find()`와 `replace()` 메소드를 사용하여 원하는 문자열을 검색하고 바꾸는 작업을 수행할 수 있습니다. 예를 들어, 위에서 만든 `text` 문자열에서 "world"라는 단어를 "universe"로 바꾸기를 원한다면 다음과 같이 할 수 있습니다.

```C++
text.replace(text.find("world"), 5, "universe");
```
위 코드에서 `find()` 메소드는 "world" 문자열이 시작되는 인덱스를 반환하고, 그 값을 `replace()` 메소드의 첫 번째 매개변수로 전달합니다. 두 번째 매개변수는 바꾸고자 하는 문자열의 길이를 나타내며, 세 번째 매개변수는 바꿀 문자열을 지정합니다.

위 코드를 실행하면 `text` 변수의 값은 "Hello, universe!"로 바뀔 것입니다.

## 깊게 들어가기

검색과 바꾸기 작업은 단순한 예제로서 소개되었지만, 더 복잡한 경우에는 정규 표현식을 사용하여 패턴을 찾을 수도 있습니다. 또한 다중 파일에서 검색과 바꾸기 작업을 수행할 수도 있습니다.

또한, C++에서는 `ifstream`와 `ofstream` 클래스를 사용하여 파일에서 텍스트를 읽어오고 수정된 텍스트를 다시 저장할 수 있습니다. 이를 활용하면 대용량의 텍스트 파일에서도 효율적으로 검색과 바꾸기 작업을 수행할 수 있습니다.

## 참고

- [C++ string 클래스 문서](http://www.cplusplus.com/reference/string/string/)
- [C++ file I/O 문서](http://www.cplusplus.com/doc/tutorial/files/) 
- [정규 표현식 튜토리얼](https://www.regular-expressions.info/tutorial.html)