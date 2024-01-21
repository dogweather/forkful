---
title:                "임시 파일 생성하기"
date:                  2024-01-20T17:40:14.177051-07:00
model:                 gpt-4-1106-preview
simple_title:         "임시 파일 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 쓰나요?)
임시 파일 생성은 데이터를 일시적으로 저장하기 위해 사용합니다. 프로그래머는 주로 테스트, 데이터 교환, 또는 큰 파일 처리 시 임시 저장공간으로 사용하기 위해 이를 생성합니다.

## How to: (어떻게 하나요?)
```C++
#include <cstdio>
#include <filesystem>

int main() {
    // 임시 파일 생성
    char temp_name[] = "tempfile-XXXXXX"; // XXXXXX 부분이 고유한 문자열로 대체됩니다.
    int file_descriptor = mkstemp(temp_name);
    
    if (file_descriptor != -1) {
        // 성공 메시지 출력
        printf("임시 파일이 생성되었습니다: %s\n", temp_name);
        
        // 파일 사용 후 제거
        std::filesystem::remove(temp_name);
    } else {
        // 에러 메시지 출력
        perror("임시 파일을 생성할 수 없습니다");
    }
    return 0;
}
```

**출력 예제**:
```
임시 파일이 생성되었습니다: tempfile-1b2e34
```

## Deep Dive (심층 분석)
임시 파일은 자동 삭제가 필요 없는 경우에 주로 사용됩니다. 과거에는 주로 `tmpnam` 또는 `tempnam` 함수를 사용했지만, 이런 함수들은 보안 문제로 권장되지 않습니다. C++17 부터는 `<filesystem>` 라이브러리의 `std::filesystem::temp_directory_path`함수를 통해 현재 시스템의 임시 디렉토리 경로를 얻을 수 있으며 이를 활용해 파일 경로를 생성할 수도 있습니다.

POSIX 시스템에서는 `mkstemp` 함수가 자주 쓰이며 고유한 파일명을 생성하고 해당 파일을 안전하게 열 수 있습니다. 파일이 열렸다면 반환된 파일 디스크립터를 통해 안전하게 파일 I/O 작업을 할 수 있습니다. 고유한 파일명 생성이 보안상 중요한 이유는 누군가가 예측 가능한 이름을 알고 있다면, 파일 생성 전에 미리 그 이름을 사용해 악의적인 공격을 할 수 있기 때문입니다.

`<cstdio>` 라이브러리는 C 기반 함수를 제공하며, C++에서는 이 라이브러리를 이용해서도 파일 작업을 할 수 있습니다. 하지만 C++은 자체 파일 핸들링 기능을 더 많이 제공하니, 여러분의 프로젝트 요구사항에 맞춰 적합한 방식을 선택하세요.

## See Also (참고 자료)
- C++ Filesystem Library: https://en.cppreference.com/w/cpp/filesystem
- C++ mkstemp: https://www.cplusplus.com/reference/cstdio/tmpfile/
- C++ 보안 강화된 임시 파일 생성: https://www.cplusplus.com/articles/j3wTURfi/