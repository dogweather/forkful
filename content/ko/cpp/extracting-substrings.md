---
title:                "하위 문자열 추출"
html_title:           "C++: 하위 문자열 추출"
simple_title:         "하위 문자열 추출"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇이며 왜?

추출하는 부분 문자열이란 무엇인지 알아보겠습니다. 프로그래머들이 이를 하는 이유는 무엇일까요? 

추출하는 부분 문자열은 기존의 문자열에서 일부분을 추출하여 따로 사용하기 위한 것입니다. 예를 들어, 한 문장에서 특정 단어만 뽑아내거나, 파일 이름에서 확장자만 추출하는 등의 작업에 주로 사용됩니다. 프로그래머들은 이 기능을 활용하여 더 효율적인 코딩을 할 수 있으며, 코드의 가독성을 높이는 데에도 도움이 됩니다.

# 방법:

## 문자열에서 특정 단어 추출하기 
```C++
string sentence = "안녕하세요! 제 이름은 존이고, 만나서 반가워요!";
string name = sentence.substr(13, 2);
```
위의 코드는 변수 `sentence`에 저장된 문자열에서 `substr()` 함수를 이용해 13번째부터 2개의 글자를 추출하여 변수 `name`에 저장합니다. 따라서 `name`에는 "존"이라는 문자열이 저장됩니다.

## 파일 이름에서 확장자 추출하기
```C++
string filename = "image.png";
string extension = filename.substr(6, 3);
```
위의 코드는 변수 `filename`에 저장된 파일 이름에서 확장자 부분을 추출하여 변수 `extension`에 저장합니다. 따라서 `extension`에는 "png"라는 문자열이 저장됩니다.

# 깊이 파고들기:

## 역사적 맥락
추출하는 부분 문자열은 1979년에 미국의 덴마크 출신 프로그래머 브라이언 커니항(Brian Kernighan)과 체스터 노선(Chet Ramey)이 개발한 유닉스 운영체제의 쉘 프로그래밍 언어(bourne shell)에서 처음 사용되었습니다. 그 이후 C언어 및 다양한 프로그래밍 언어에서도 이 기능이 채용되었습니다.

## 대안
추출하는 부분 문자열 기능 외에도 정규표현식(Regular Expression)을 이용하여 문자열에서 원하는 부분을 추출할 수 있습니다. 정규표현식은 더 복잡한 패턴을 이용하여 문자열을 추출할 수 있고, 보다 다양한 문자열 처리 기능을 제공합니다.

## 구현 세부사항
추출하는 부분 문자열 기능은 `string` 라이브러리의 `substr()` 함수를 이용하여 구현할 수 있습니다. `substr()` 함수는 두 개의 매개변수를 받는데, 첫 번째 매개변수는 추출을 시작할 인덱스를, 두 번째 매개변수는 추출할 문자의 개수를 나타냅니다. 또한 C++11부터는 `substr()` 함수에 시작 인덱스 대신, 추출을 시작할 위치를 나타내는 `pos` 매개변수를 이용할 수도 있습니다.

# 관련 자료:

- [C++ string class](https://www.cplusplus.com/reference/string/string/)
- [C++11 Library Reference](https://docs.microsoft.com/en-us/cpp/standard-library/string-class?view=vs-2019)
- [Regular Expressions in C++](https://www.regular-expressions.info/cpp.html)