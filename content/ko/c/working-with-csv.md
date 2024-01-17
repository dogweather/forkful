---
title:                "CSV 작업하기"
html_title:           "C: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-csv.md"
---

{{< edit_this_page >}}

# CSV 작업하는 방법과 이유
CSV는 Comma Separated Values의 약자로, 콤마로 구분되어있는 텍스트 파일 형태입니다. 프로그래머들은 CSV를 사용하는 이유는 데이터를 저장하고 공유하기 위해서입니다. CSV는 엑셀 등 다른 프로그램에서도 쉽게 사용할 수 있어서 데이터 조작에 활용하기에 용이합니다.

# 어떻게 하나요?

```C
#include <stdio.h>
#include <string.h>

int main() {
    // CSV 파일 열기
    FILE *fp = fopen("sample.csv", "r");

    // 파일에서 문자열 읽기
    char str[100];
    while(fgets(str, 100, fp) != NULL) {
        // 문자열에서 콤마로 나누기
        char *token = strtok(str, ",");

        // 각 항목 출력
        while(token != NULL) {
            printf("%s", token);

            // 다음 항목으로 이동
            token = strtok(NULL, ",");
        }
    }

    // 파일 닫기
    fclose(fp);
    return 0;
}
```

### 코드 설명:
- 파일을 열고, fgets 함수를 사용해 파일에서 문자열을 읽어옵니다.
- 문자열을 strtok 함수를 이용해 콤마로 나누고, 각 항목을 출력합니다.
- 파일을 닫아줍니다.

### 샘플 출력:
- sample.csv 파일 내용: Alex,25,Programmer,Hobby: Coding
- 출력: Alex
    25
    Programmer
    Hobby: Coding

# CSV 깊이 파보기
- CSV는 1972년 Trinity 컴퓨터에서 최초로 사용되었습니다.
- CSV는 다른 형식인 XML보다 용량이 작고 읽고 쓰기가 쉬워서 많이 사용됩니다.
- 이 예제에서는 콤마로 데이터를 나누었지만, 다른 구분 기호도 사용할 수 있습니다. 예를 들어, 탭이나 세미콜론을 사용할 수 있습니다.

# 관련 링크
- [strtok 함수 정보](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [CSV 파일 형식 정보](http://www.ietf.org/rfc/rfc4180.txt)
- [다른 구분 기호를 사용하는 예제](https://cboard.cprogramming.com/c-programming/162166-alternative-delimiters-strtok.html)