---
title:                "C: csv로 작업하기"
simple_title:         "csv로 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
CSV란 무엇이며 왜 사용해야 할까요?

CSV(Comma Separated Values)은 데이터를 저장하기 위한 텍스트 파일 형식입니다. 이것은 각 데이터를 콤마(,)로 구분하여 저장하는 방식으로, 일반적으로 스프레드시트 프로그램에서 많이 사용됩니다. CSV 파일은 구조화된 데이터를 손쉽게 읽고 쓸 수 있기 때문에 데이터 분석 및 처리에 매우 유용합니다.

따라서 C 프로그래밍을 배우고 있거나 데이터 분석에 관심이 있는 독자라면, CSV 파일을 다루는 방법을 알아두는 것이 좋습니다.

## 방법
아래의 예제 코드를 통해 CSV 파일을 읽고 쓰는 방법을 알아보겠습니다.

```C
#include <stdio.h>
#include <stdlib.h>

// CSV 파일에서 데이터를 읽어오는 함수
void read_csv(char *filename)
{
    FILE *fp = fopen(filename, "r");
    char line[256];

    // 한 줄씩 읽어서 출력
    while (fgets(line, sizeof(line), fp) != NULL)
    {
        printf("%s", line);
    }

    fclose(fp);
}

// CSV 파일에 데이터를 쓰는 함수
void write_csv(char *filename)
{
    FILE *fp = fopen(filename, "w");

    // 3x3 크기의 행렬 데이터
    int matrix[3][3] = { {1, 2, 3},
                         {4, 5, 6},
                         {7, 8, 9} };

    // 각 셀을 콤마로 구분하여 한 줄에 쓰기
    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            fprintf(fp, "%d,", matrix[i][j]);
        }
        // 한 줄 끝에 콤마 대신 개행문자 출력
        fprintf(fp, "\n");
    }

    fclose(fp);
}

// main 함수
int main(void)
{
    // 샘플 CSV 파일 이름
    char *filename = "sample.csv";

    // CSV 파일 읽기
    printf("Reading CSV file...\n");
    read_csv(filename);

    // CSV 파일 쓰기
    printf("Writing CSV file...\n");
    write_csv(filename);

    // 쓰여진 CSV 파일 다시 읽기
    printf("Reading CSV file again...\n");
    read_csv(filename);

    return 0;
}
```

위의 코드를 실행하면 "sample.csv" 파일에 아래와 같은 내용이 쓰여질 것입니다.

```
1,2,3,
4,5,6,
7,8,9,
```

또한 "sample.csv" 파일을 다시 읽어오면 위의 내용이 출력될 것입니다.

## 딥 다이브
CSV 파일을 다루는 더 깊은 지식을 알고 싶다면 아래의 링크를 참고하시기 바랍니다.

* [C언어에서 파일 입출력을 하기 위한 fopen() 함수](https://modoocode.com/24)
* [C언어에서 문자열 처리를 위한 함수들](https://modoocode.com/27)
* [CSV 파일 형식 설명 및 예제](https://support.microsoft.com/ko-kr/help/214330/description-of-csv-comma-separated-values)

## 더 알아보기
[CSV 파일 처리 라이브러리를 사용하는 방법](https://modoocode.com/112)
[데이터 분석을 위한 C 프로그래밍 관련 자료](https://weejw.tistory.com/337)