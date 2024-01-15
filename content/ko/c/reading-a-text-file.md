---
title:                "텍스트 파일 읽기"
html_title:           "C: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?
컴퓨터 프로그래밍을 배우면 다양한 유형의 파일을 읽고 쓸 수 있게 됩니다. 이 중에서도 텍스트 파일을 읽는 것은 가장 기초적이면서도 필수적인 기능입니다. 파일을 읽는 프로그램을 작성하면서 이 작업이 왜 중요하고 어떻게 할 수 있는지 알아보겠습니다.

## 어떻게 하나요?
텍스트 파일을 읽는 것은 C 프로그래밍에서 기본적이고 중요한 기능 중 하나입니다. 이를 위해 다음과 같은 코드를 사용할 수 있습니다.
```C
#include <stdio.h>

int main() {
   FILE *fp;
   char str[100];

   /* 파일 열기 */
   fp = fopen("test.txt", "r");

   /* 파일에서 문자열 읽기 */
   fgets(str, 100, fp);
   printf("읽은 문자열: %s", str);

   /* 파일 닫기 */
   fclose(fp);

   return 0;
}
```
위 코드는 "test.txt" 파일을 읽고 파일에서 문자열을 읽어서 화면에 출력하는 예제입니다. 만약 파일을 열지 못한다면 적절한 오류 메시지가 표시됩니다.

## 딥 다이브
텍스트 파일을 읽는 것은 기초적인 작업이지만, 모든 파일을 읽을 수 있는 것은 아닙니다. 파일이 잘못 되었거나 다른 형식의 파일일 경우에는 적절한 에러 처리가 필요합니다. 또한 파일의 크기가 너무 크면 한 번에 모두 읽지 못할 수도 있고, 파일에 문자열과 숫자가 혼합되어 있는 경우에는 문제가 발생할 수 있습니다. 이러한 상황을 고려하여 적절한 예외 처리를 해주어야 합니다.

## 그 외 참고 자료
- [C 언어 공식 문서](https://ko.wikipedia.org/wiki/C_%EC%96%B8%EC%96%B4)
- [텍스트 파일 읽는 방법 설명 영상](https://www.youtube.com/watch?v=sXW2VLrQ3Bs)
- [파일 입출력에 대한 자세한 설명](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

---
## 더 보기
- [C 파일 입출력 예제 코드 모음](https://www.programiz.com/c-programming/c-file-input-output)