---
title:                "텍스트 파일 작성하기"
html_title:           "C: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 파일을 작성하는 것이란 무엇일까요? 그것은 단순하게 말하면 텍스트 데이터를 저장하는 파일입니다. 프로그래머들이 이를 하는 이유는 이러한 텍스트 파일을 이용하여 다양한 정보를 저장하고 불러오기 위해서입니다.

## 방법:
아래에 ```C ... ``` 코드 블록 안에 코딩 예제와 샘플 출력을 함께 제공하겠습니다. 이를 이용하여 텍스트 파일을 작성하는 방법을 배워보세요.

```c
#include <stdio.h>
int main(){
    // 파일을 쓰기 모드로 연다.
    FILE* text_file = fopen("sample.txt", "w");

    // 텍스트 파일에 쓸 내용 작성하기
    fprintf(text_file, "안녕하세요! 이것은 텍스트 파일입니다.");

    // 파일 닫기
    fclose(text_file);

    return 0;
}

```

## 깊이 파고들기:
텍스트 파일을 작성하는 방법을 알아봤으니, 조금 더 자세히 살펴보도록 하겠습니다. 텍스트 파일은 보통 ASCII, UTF-8 또는 UTF-16과 같은 문자 인코딩을 이용하여 저장됩니다. 이를 이해하기 위해서는 문자 인코딩과 관련된 몇 가지 개념을 알고 있어야 합니다. 또한, 다른 파일 형식을 이용하여 데이터를 저장하는 방법도 있습니다. 예를 들어, 바이너리 파일은 텍스트 파일과 달리 특정 데이터 구조를 따라 데이터를 저장하는 방식으로 사용됩니다.

## 관련 자료:
텍스트 파일 작성에 대해 더 알고 싶다면 아래 링크를 참고해보세요.
- [파일 입출력(C)](https://modoocode.com/37)
- [UTF-8(위키피디아)](https://ko.wikipedia.org/wiki/UTF-8)
- [바이너리 파일(위키피디아)](https://ko.wikipedia.org/wiki/%EB%B0%94%EC%9D%B4%EB%84%88%EB%A6%AC_%ED%8C%8C%EC%9D%BC)