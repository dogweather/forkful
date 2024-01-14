---
title:                "C: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 할 때, 우리는 종종 특정 문자열을 필요로 합니다. 하지만 때로는 그 문자열의 일부만 필요한 경우가 있습니다. 이럴 때 서브스트링 추출이 필요합니다. 이 기술을 사용하면 원하는 부분만 가져와서 다른 작업에 사용할 수 있습니다.

## 하는 방법

서브스트링 추출은 C 프로그래밍에서 매우 유용한 기술입니다.

### 첫 번째 방법

먼저, 우리는 문자열과 추출하고 싶은 시작 인덱스와 마지막 인덱스를 정의해야 합니다. 그 다음, 해당 문자열의 시작 인덱스부터 끝 인덱스까지 반복문을 사용하여 문자를 하나씩 가져오면 됩니다.

```C
char string[] = "안녕하세요";
int start_index = 2;
int end_index = 4;

for (int i = start_index; i <= end_index; i++) {
  printf("%c", string[i]);
}

// 출력: 녕하
```

### 두 번째 방법

또 다른 방법으로는 `memcpy()` 함수를 사용하는 것입니다. 이 함수는 주어진 두번째 문자열의 시작 위치부터 첫번째 문자열에 지정된 길이만큼 복사합니다.

```C
char string[] = "안녕하세요";
char substring[4];

memcpy(substring, &string[2], 4); // substring에 "녕하" 복사

printf("%s", substring); // 출력: 녕하
```

## 깊게 들어가기

서브스트링 추출은 문자열을 조작하는 다양한 기술 중 하나입니다. 이 기술을 사용하여 문자열을 잘라내는 것 외에도, 문자열 내부의 특정 위치에 문자를 삽입하거나, 검색하여 특정 문자열을 바꾸는 등 다양한 작업을 수행할 수 있습니다.

더 많은 예제와 기술은 [여기를](https://www.studytonight.com/c/string-manipulation-in-c.php) 참조하세요.

## 또 보기

- [C 언어 재입문하기](https://ko.wikipedia.org/wiki/C_언어)
- [C 언어 문법 할렐루야!](https://www.inflearn.com/course/c언어-영상버전-2)