---
title:    "C: 텍스트 파일 쓰기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 데이터를 저장하고 공유하기 위함입니다.

## 어떻게

먼저, 새로운 파일을 열어서 `fopen()` 함수를 사용하여 파일에 대한 포인터를 만듭니다. 그리고 `fputs()` 함수를 사용하여 원하는 데이터를 파일에 입력합니다. 파일을 다 사용한 후에는 `fclose()` 함수를 사용하여 포인터를 닫아줍니다.

```C
FILE *fp;
fp = fopen("example.txt", "w");
fputs("This is an example text file.", fp);
fclose(fp);
```

## 딥다이브

텍스트 파일을 작성하는 것은 매우 중요합니다. 파일을 작성할 때에는 적절한 형식으로 데이터를 입력해야 합니다. 또한 파일에 대한 포인터를 올바르게 관리하여 메모리 누수를 방지해야 합니다. 이를 위해서 `fopen()` 함수에서 리턴되는 포인터를 항상 체크하고, 파일을 닫을 때에는 `fclose()` 함수를 호출하는 것이 좋습니다.

## 참고

- [C 파일 입출력 - KMOOC 교육자료](https://kaist.edx.kr/courses/course-v1:KAISTX+AC101x+3T2019/course/) 
- [The C Programming Language - 브라이언 커니핸(Brian Kernighan), 데니스 리치(Dennis Ritchie)](https://www.aladin.co.kr/shop/wproduct.aspx?ItemId=45009430)