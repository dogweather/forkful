---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용합니까? 
임시 파일 생성은 애플리케이션이 동작하는 동안 발생하는 중간 데이터를 일시적으로 저장하는 방법입니다. 이는 프로그램 오류 시 데이터 손실을 방지하며, 데이터 분석 및 디버깅에 유용하게 사용됩니다.

## 어떻게:
다음은 C++에서 임시 파일을 생성하는 예시 코드입니다.

```C++
#include <cstdio>

int main() {
    char tempFileName[L_tmpnam] = {0}; 
    tmpnam( tempFileName );
    printf( "생성된 임시 파일: %s\n", tempFileName );
}
```

매번 실행할 때마다 서로 다른 파일 이름이 출력됩니다.

```bash
생성된 임시 파일: /tmp/gyw294v
생성된 임시 파일: /tmp/xyz382h
```
## 깊은 내용: 
 * **역사적 맥락**: 프로그래밍 초기 단계에서, 임시 파일은 소프트웨어 오류 대비를 위해 중요한 역할을 했습니다. 데이터 저장의 물리적 한계 때문에 메모리 유실이 잦아, 임시 파일이 보안 역할을 했습니다.

 * **대안들**: 대안으로는 메모리에 직접 데이터를 쓰고 읽는 '메모리 맵 파일'이 있습니다. 하지만 이렇게 하면 쓰기 메모리에 제한이 생각보다 크기 때문에 임시 파일이 여전히 중요합니다.

 * **구현 상세**: C++에서는 파일을 삭제하지 않고도 임시 파일을 생성할 수 있으며, 이는 프로그램이 종료될 때 자동으로 삭제됩니다. 이것은 이러한 파일을 다룰 때 발생할 수 있는 유지 관리 작업을 최소화합니다.

## 참고 자료: 
1. [Creating and Using a Temporary File](https://www.cplusplus.com/reference/cstdio/tmpnam/)
2. [Working with Temporary Files](https://www.ibm.com/docs/en/zos/2.4.0?topic=operations-working-with-temporary-files)
3. [Memory-Mapped Files](https://en.cppreference.com/w/cpp/memory/shared_ptr)