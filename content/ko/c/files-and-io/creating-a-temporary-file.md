---
title:                "임시 파일 생성하기"
aliases: - /ko/c/creating-a-temporary-file.md
date:                  2024-02-03T17:55:32.262726-07:00
model:                 gpt-4-0125-preview
simple_title:         "임시 파일 생성하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
C에서 임시 파일을 생성하는 것은 데이터 처리나 저장을 위한 임시 공간으로 일반적으로 사용되는 파일을 생성하는 것을 의미합니다. 프로그래머들은 프로그램의 영구 저장소에 영향을 주지 않으면서 임시 데이터를 관리하거나 사용 후 민감한 데이터가 지워지도록 하기 위해 이 작업을 합니다.

## 방법:
C 프로그래밍 언어에서 임시 파일을 생성하는 것은 `tmpfile()` 및 `mkstemp()`와 같은 함수를 활용할 수 있습니다.

**`tmpfile()` 사용하기**: 이 함수는 프로그램이 종료되거나 파일이 닫힐 때 자동으로 삭제되는 고유한 임시 파일을 생성합니다.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Failed to create temporary file");
        return 1;
    }

    // 임시 파일에 데이터 쓰기
    fputs("This is a test.\n", temp);

    // 돌아가서 우리가 쓴 것을 읽기
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // 닫기 또는 프로그램 종료시 자동 삭제
    fclose(temp);

    return 0;
}
```
**샘플 출력:**
```
This is a test.
```

**`mkstemp()` 사용하기**: 임시 파일의 위치와 권한을 더 잘 제어할 수 있습니다. `XXXXXX`로 끝나는 템플릿 문자열을 필요로 하며, 그런 다음 이름 충돌을 방지하기 위해 고유한 시퀀스로 대체합니다.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Failed to create temporary file");
        return 1;
    }
    
    printf("Temporary file created: %s\n", template);

    // mkstemp()로 생성된 임시 파일은 수동으로 삭제해야 합니다.
    unlink(template);

    close(fd);
    return 0;
}
```
**샘플 출력:**
```
Temporary file created: /tmp/mytemp-abc123
```

## 심층 탐구
임시 파일의 개념은 C에만 고유한 것이 아니며, 순간적인 데이터 처리에 있어서 그 유용성 때문에 많은 프로그래밍 환경에서 공통적인 기능입니다. ISO C 표준에 표준화된 `tmpfile()` 함수는 표준 디렉토리에서 고유한 이름을 가진 파일을 생성하지만, 그 존재는 덧없어서 보안이나 임시 작업에 이상적입니다.

`tmpfile()`의 주목할 만한 한계 중 하나는 기본 임시 디렉토리에 의존한다는 것으로, 모든 어플리케이션에 적합하지 않을 수 있습니다. 특히 권한이나 보안 측면에서 문제가 될 수 있습니다. 반면, `mkstemp()`는 디렉토리를 지정할 수 있고 제공된 템플릿 문자열을 수정하여 고유한 파일명을 보장함으로써 안전한 파일 생성을 보장하며, 수동 파일 관리의 수고로움을 감수하면서 더 다양하게 해결할 수 있는 방법을 제공합니다.

그러나, 임시 파일을 생성할 때는 적절하게 처리하지 않으면 보안 취약점, 예를 들어 경쟁 조건과 같은 문제가 발생할 수 있습니다. 예를 들어, `tmpfile()`과 `mkstemp()`는 보안 임시 파일 생성의 다른 측면(자동 삭제 및 안전한 이름 생성, 각각)을 다루지만, 어느 것도 만병통치약은 아닙니다. 개발자는 임시 파일로 인해 도입될 수 있는 애플리케이션 보안 요구 사항의 구체적인 사항을 고려해야 하며, 이러한 기능이 제공하는 것 이상의 추가적인 보호 조치를 구현할 필요가 있을 수 있습니다.

프로그래밍의 더 넓은 풍경에서, 임시 데이터 처리를 위해 더 나은 성능이나 보안을 제공할 수 있는 메모리 내 저장 (예: 동적 데이터 구조 또는 메모리 매핑 파일 사용)과 같은 대안이 있을 수 있습니다. 그럼에도 불구하고, 대규모 데이터 세트를 다루거나 프로세스 간 통신이 관련된 경우와 같은 많은 시나리오에서 물리적인 임시 파일은 여전히 중요한 도구입니다.
