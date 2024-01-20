---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 체크하는 것은 파일 시스템 내 특정 디렉토리의 존재 유무를 확인하는 프로그래밍 테크닉입니다. 이는 유효한 파일 경로를 처리하거나 존재하지 않는 디렉토리를 만들기 전 불필요한 오류를 방지하기 위해 중요합니다.

## 어떻게:

예시 코드와 출력 결과는 다음과 같습니다:

```C
#include<stdio.h>
#include<unistd.h>

int main() {
   if(access("/mydirectory", F_OK ) != -1) {
      printf("Directory exists.\n");
   } else {
      printf("Directory doesn't exist.\n");
   }
   return 0;
}
```
이 코드의 결과는 "/mydirectory"라는 디렉토리가 존재하면 "Directory exists."를 출력하고, 그렇지 않으면 "Directory doesn't exist."를 출력하는 것입니다.

## 깊이 들어가기:

디렉토리 존재 여부를 확인하는 것은 Unix 시스템에서부터 시작된 표준 테크닉으로, 상대적으로 대부분의 운영 체제에서 사용됩니다.

대안적으로, `stat` 함수 또는 `opendir` 함수를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 그러나 `access` 함수는 `F_OK` 플래그를 사용하여 디렉토리의 존재를 빠르게 확인할 수 있으므로 효율적입니다.

C 언어에서 디렉토리를 처리할 때는 파일 시스템의 특성을 이해하고 적절한 에러 처리를 수행하는 것이 중요하다는 점을 항상 기억해야 합니다.

## 참고:

다음은 이 주제와 관련된 링크입니다:

- Chmod, Change Mode의 이해: <http://bit.ly/chmodUnderstanding>
- 파일 또는 디렉토리의 존재 확인하는 방법을 자세히 설명: <http://bit.ly/checkExistence>
- 파일 시스템과 관련 기타 함수에 대한 C Library Tutorial: <http://bit.ly/ClibTutorial>