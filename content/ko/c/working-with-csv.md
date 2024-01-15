---
title:                "csv 파일로 작업하기"
html_title:           "C: csv 파일로 작업하기"
simple_title:         "csv 파일로 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜?

어떤 프로그래밍을 하든 데이터를 다루는 일은 아주 중요합니다. 그 중에서도 CSV 파일은 간단하게 다룰 수 있고 널리 사용되기 때문에 프로그래머들에게 인기가 많습니다. CSV 파일을 다루는 법을 배우는 것은 프로그래밍에 있어서 유용한 기술입니다.

## 코드로 배우는 CSV 파일 다루는 법

CSV 파일은 데이터를 쉼표(,)로 구분한 파일입니다. 이렇게 간단한 형식이지만 많은 데이터를 담을 수 있으며, 프로그래머 입장에서도 쉽게 다룰 수 있습니다. 아래의 예시 코드를 통해 CSV 파일을 읽고 쓰는 방법을 살펴보겠습니다.

```C
#include <stdio.h>

int main() {
   
    // CSV 파일 열기
    FILE *fp = fopen("example.csv", "r");

    // 한 줄씩 읽어오기
    char line[100];
    while (fgets(line, 100, fp) != NULL) {
        
        // 데이터 분리하기
        char *token;
        token = strtok(line, ",");

        // 데이터 출력하기
        while (token != NULL) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }

    // 파일 닫기
    fclose(fp);
}
```
위 코드는 "example.csv"라는 이름의 CSV 파일을 열고, 한 줄씩 읽어오고, 데이터를 쉼표를 기준으로 분리하여 출력하는 예시입니다. 만약 CSV 파일에 "Hello,World"라는 데이터가 있다면, "Hello"와 "World"가 각각 다른 줄에 출력됩니다.

## CSV 파일 다루는 더 깊은 내용

위에서 살펴본 예시 코드를 통해 기본적인 CSV 파일 다루는 방법을 배웠습니다. 하지만 이 외에도 CSV 파일 안에는 쉼표가 아니라 다른 문자로 구분되는 경우도 있을 수 있습니다. 이 경우에는 코드를 약간 수정해야 합니다. 또한, CSV 파일을 다루기 전에 파일을 읽고 쓰는 방법부터도 알아야 합니다.

## 참고

- [C 파일 다루는 법](https://modoocode.com/204)
- [strtok 함수 설명](https://www.ibm.com/support/knowledgecenter/ko/ssw_ibm_i_74/rtref/strtok.htm)
- [CSV 파일 다루기 예시 코드](https://www.cprogramming.com/tutorial/cfileio.html)