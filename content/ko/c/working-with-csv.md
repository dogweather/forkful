---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (CSV란 무엇이며 왜 사용하는가?)

CSV(Comma-Separated Values)는 데이터를 저장하고 교환하기 위한 텍스트 형식입니다. 쉼표로 구분된 값들은 테이블 형태의 데이터를 간단하게 표현하게 해서 데이터베이스와 스프레드시트가 손쉽게 교환될 수 있게 합니다.

## How to: (실제 적용 방법)

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("File opening failed.");
        return EXIT_FAILURE;
    }

    char buffer[1024];
    while (fgets(buffer, 1024, fp)) {
        // strtok 등을 사용해 쉼표로 구분한 값을 파싱하세요.
        printf("%s", buffer);
    }

    fclose(fp);
    return 0;
}
```
**[출력 예시]**
```
name,age,gender
Alice,30,F
Bob,25,M
```

## Deep Dive (깊이 알아보기)

CSV는 1970년대부터 사용되어 왔으며, 간단한 텍스트 혹은 로그 파일로 다룰 수 있는 가장 직관적인 자료 형식 중 하나입니다. JSON이나 XML 같은 현대적인 형식과 비교했을 때, CSV는 가독성이 떨어질 수 있지만, 파일 크기가 작고 파싱이 간단해서 여전히 널리 사용됩니다. 파싱은 `strtok` 같은 문자열 함수를 사용하거나, 더 복잡한 데이터 구조를 위해서 CSV 파싱 라이브러리를 활용할 수도 있습니다.

## See Also (관련 자료 링크)

- [RFC 4180](https://tools.ietf.org/html/rfc4180): CSV 파일 형식의 표준 정의
- [libcsv](http://sourceforge.net/projects/libcsv/): 오픈 소스 CSV 파싱 라이브러리
- [Stack Overflow](https://stackoverflow.com/questions/tagged/csv?sort=votes&pagesize=15): CSV 관련 질문 및 답변
