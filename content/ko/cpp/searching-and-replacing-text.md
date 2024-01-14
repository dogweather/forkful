---
title:                "C++: 텍스트 검색 및 대체하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 것에 참여하는 이유는 매우 간단합니다 - 우리가 프로그래밍하는 과정에서 문자열을 다루는 일이 매우 일반적이기 때문입니다. 우리는 종종 특정 문자열을 찾아서 다른 문자열로 바꾸고 싶을 때가 있습니다. 이를 위해서는 간단하면서도 강력한 검색 및 바꾸기 기능이 필요합니다.

## 방법

이제 우리는 C++을 사용하여 텍스트 검색 및 바꾸기를 수행하는 방법을 알아보겠습니다. 먼저, ```std::string``` 클래스의 ```find()```와 ```replace()``` 함수를 사용하여 문자열을 검색하고 바꿀 수 있습니다.

예를 들어, 다음과 같은 문자열을 가지고 있는 상황을 상상해보겠습니다:

```C++
std::string text = "안녕하세요, 내 이름은 Jake입니다.";
```

우리는 이 문자열에서 "Jake"를 "John"으로 바꾸고 싶을 수 있습니다. 이를 위해서는 먼저 "Jake"가 어디에 위치하는지를 파악해야 합니다. 이를 위해 ```find()``` 함수를 사용할 수 있습니다.

```C++
int index = text.find("Jake");
```

이제 우리는 "Jake"가 위치하는 인덱스를 변수에 저장할 수 있습니다. 이제 우리는 이 인덱스를 사용하여, ```replace()``` 함수를 이용해 바꿀 수 있습니다.

```C++
text.replace(index, 4, "John");
```

여기서 첫 번째 매개변수는 문자열을 바꿀 시작 위치이며, 두 번째 매개변수는 바꿀 문자열의 길이입니다. 마지막 매개변수는 바꿀 문자열 자체입니다.

위의 두 코드를 합치면, 다음과 같은 결과가 나옵니다:

```
안녕하세요, 내 이름은 John입니다.
```

## 딥 다이브

위의 예시는 매우 간단한 경우입니다. 하지만 좀 더 깊이 들어가보면 더 많은 옵션이 있습니다. 예를 들어, ```replace()``` 함수의 매개변수 중 하나인 ```count```는 바꿀 횟수를 지정할 수 있습니다. 또한, 문자열을 검색할 때 패턴을 사용할 수도 있습니다. 이를 위해 정규 표현식(regular expressions)을 사용할 수 있습니다.

더 많은 정보를 알고 싶다면 다음 링크들을 참고해보세요.

## 참고

- [C++ string 클래스 살펴보기](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++ 함수 검색 및 바꾸기 예제](https://www.programiz.com/cpp-programming/library-function/string/replace)
- [C++ 정규 표현식 활용하기](https://www.geeksforgeeks.org/regular-expressions-in-c-with-examples/)

더 많은 프로그래밍 관련 정보를 찾고 싶다면 아래 블로그를 방문해보세요!

## 관련 사이트

- [WernerTech (워너테크)](https://wernertech.com/)
- [GoormIDE (구름아이디)](https://ide.goorm.io/)
- [Naver D2 (네이버 디투)](https://d2.naver.com/home)