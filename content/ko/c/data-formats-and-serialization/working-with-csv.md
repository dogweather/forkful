---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:51.084661-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:55.962360-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:


### CSV 파일 읽기
C에서 CSV 파일을 읽기 위해, 우리는 표준 파일 입출력 함수와 더불어 각 라인을 파싱하기 위한 문자열 조작 함수를 사용합니다. 아래는 CSV 파일을 읽고 각 행의 필드를 콘솔에 출력하는 기본 예제입니다.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
샘플 `data.csv`:
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

샘플 출력:
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### CSV 파일 쓰기
마찬가지로, CSV 파일에 쓰기는 쉼표로 구분된 형식으로 데이터를 저장하기 위해 `fprintf`를 사용하는 것을 포함합니다.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

샘플 `output.csv` 내용:
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## 심층 탐구
CSV 형식은 겉보기에는 직관적이지만, 필드 내에 쉼표를 다루고 필드를 따옴표로 묶는 것과 같은 미묘한 부분이 있습니다. 위에 보여진 기초적인 예제들은 이러한 복잡성을 적절하게 다루지 않으며, 잠재적인 오류를 강건하게 처리하지도 않습니다.

역사적으로, C에서의 CSV 처리는 이 언어의 저수준 특성과 이러한 작업을 위한 내장 고수준 추상화의 부재로 인해 대부분 수동으로 이루어졌습니다. 이 수동 관리에는 파일 열기, 라인 읽기, 문자열 분할, 필요에 따른 데이터 유형 변환 등이 포함됩니다.

C에서 직접적으로 CSV 파일을 조작하는 것은 파일 입출력과 문자열 처리에 대한 소중한 학습 경험을 제공하지만, `libcsv` 및 `csv-parser`와 같은 라이브러리는 따옴표로 묶인 필드와 사용자 정의 구분자를 포함하여 CSV 파일을 읽고 쓰는 포괄적인 기능을 제공함으로써 효율성을 약속하고 오류가 발생하기 쉬운 과정을 줄여줍니다.

대안적으로, 지원하는 생태계 내에서 작업할 때, 고수준 CSV 조작 기능(예: Python의 `pandas` 라이브러리)을 제공하는 언어나 플랫폼과 통합하는 것이 무거운 CSV 처리가 필요한 응용 프로그램에 대해 보다 생산적인 경로가 될 수 있습니다. 이런 언어간 접근 방식은 C의 성능 및 시스템 프로그래밍 능력을 활용하는 동시에, CSV 처리와 같은 특정 작업을 위한 다른 언어의 사용 용이성을 활용합니다.
