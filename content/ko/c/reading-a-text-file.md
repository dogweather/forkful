---
title:                "C: 텍스트 파일 읽기"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 중요한 이유는 다양합니다. 우선, 많은 프로그램이 텍스트 파일을 읽고 사용하기 때문에 중요한 데이터 소스가 됩니다. 또한, 텍스트 파일을 읽는 것은 그 파일 안에 담겨있는 정보를 분석하고 처리할 수 있는 역량을 키워줍니다.

## 하우 투

텍스트 파일을 읽는 방법은 매우 간단합니다. 먼저, ```fopen()``` 함수를 사용해 파일을 열어줍니다. 그런 다음, ```fscanf()``` 함수를 사용해 파일에서 원하는 데이터를 읽어올 수 있습니다. 아래는 파일을 열고 내용을 출력하는 간단한 예제 코드입니다.

```C
#include <stdio.h>

int main() {
    FILE* file = fopen("sample.txt", "r"); //sample.txt라는 파일을 읽기 전용으로 열기
    char str[100]; //문자열을 넣을 버퍼 생성

    while(fscanf(file, "%s", str) != EOF) { //파일에서 문자열 읽어오기 --> 파일이 끝날 때까지 반복
        printf("%s\n", str); //읽어온 문자열 출력
    }

    fclose(file); //파일 닫기
    return 0;
}
```

위 코드를 실행하면 파일 안에 있는 데이터를 한 줄씩 출력할 수 있습니다.

## 딥 다이브

텍스트 파일을 읽는 방법은 여러 가지가 있습니다. ```fscanf()``` 함수를 사용하는 것 외에도, ```fgets()``` 함수를 사용하거나 파일 포인터를 이용해서 문자 하나씩 읽어오는 방법도 있습니다. 또한, 파일을 쓰기 모드로 열어서 텍스트를 파일 안에 쓸 수도 있습니다.

참고하실만한 추가 정보: [C 파일 처리 기본 함수들](https://modoocode.com/186), [C 파일 처리 예제 코드](https://www.dreamy.pe.kr/zbxe/CodeClip/92152)

## 더보기

[C 언어 공식 문서](https://ko.wikipedia.org/wiki/C_%EC%96%B8%EC%96%B4), [C 파일 처리 관련 자세한 내용](https://ko.wikipedia.org/wiki/C_%ED%8C%8C%EC%9D%BC_%EC%B2%98%EB%A6%AC), [C 파일 입출력 레퍼런스](https://ko.wikipedia.org/wiki/C_%ED%8C%8C%EC%9D%BC_%EC%9E%85%EC%B6%9C%EB%A0%A5_%EB%A0%88%ED%8D%BC%EB%9F%B0%EC%8A%A4)