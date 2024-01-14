---
title:    "C: 패턴과 일치하는 문자 삭제하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜
컴퓨터 프로그래밍을 하다보면 종종 문자열에서 특정한 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이 글에서는 이러한 경우를 처리하는 방법에 대해 알아보겠습니다.

## 방법
패턴 매칭 문자 삭제에 대해 다양한 방법이 있지만, 여기에서는 C 프로그래밍 언어를 사용하여 이를 수행하는 예제 코드를 제공합니다.

```C
#include <stdio.h>
#include <string.h>

int main(void) {
   char str[100];
   char pattern[20];
   char result[100];
   int i, j, k = 0, flag = 0;

   // 패턴과 일치하는 문자를 삭제할 문자열 입력
   printf("문자열 입력: ");
   fgets(str, 100, stdin);

   // 삭제할 패턴 입력
   printf("삭제할 패턴 입력: ");
   fgets(pattern, 20, stdin);

   // 원본 문자열을 확인하며 패턴과 일치하지 않는 문자를 새로운 문자열에 복사
   for (i = 0; i < strlen(str); i++) {
      for (j = 0; j < strlen(pattern); j++) {
         if (str[i] == pattern[j]) {
            flag = 1;
            break;
         }
      }
      if (flag == 0) {
         result[k] = str[i];
         k++;
      }
      flag = 0;
   }

   // 결과 출력
   printf("결과: %s\n", result);

   return 0; 
}
```

### 예제 출력
```
문자열 입력: Hello World!
삭제할 패턴 입력: l
결과: Heo Word!
```

## 깊이있게 알아보기
위의 예제 코드에서는 패턴과 일치하지 않는 문자를 새로운 문자열에 복사하는 방식으로 패턴 매칭 문자 삭제를 수행했습니다. 이 방법은 간단하지만, 원본 문자열을 수정하지 않고 새로운 문자열을 생성해야하기 때문에 메모리 사용에 있어서는 비효율적일 수 있습니다. 더 효율적인 방법으로는 원본 문자열을 직접 수정하는 방식이 있습니다. 하지만 이 경우에는 원본 문자열이 변경되기 때문에 주의해야 합니다.

## 관련 자료
- [C 프로그래밍 언어 공식 문서 (한국어)](https://ko.wikipedia.org/wiki/C_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8)
- [컴퓨터 프로그래밍 언어의 종류 (한국어)](https://blog.fastcampus.co.kr/dev_redblock2/)
- [패턴 매칭 알고리즘 (한국어)](https://ko.wikipedia.org/wiki/%EB%B0%9C%ED%80%80_%EC%9E%90%EB%B0%94%EC%9A%B4_%EB%A7%A4%EC%B9%AD_%EC%95%8C%EA%B3%A0%EB%A6%AC%EC%A6%98)

## 연관되는 링크
- [Korean Programming Blog](https://ko.programming-blog.com/)
- [The C Programming Language (English)](https://en.wikipedia.org/wiki/The_C_Programming_Language)
- [C Programming Language Tutorial (English)](https://www.cprogramming.com/tutorial/c-tutorial.html)