---
title:                "임시 파일 생성"
html_title:           "C: 임시 파일 생성"
simple_title:         "임시 파일 생성"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
임시 파일을 생성한다는 것은 무엇인가요? 프로그래머들이 이 작업을 왜 하게 될까요? 

임시 파일은 프로그램이 실행되는 동안 데이터를 저장할 수 있는 임시적인 파일입니다. 프로그램이 실행 중에 일시적으로 필요한 정보를 저장하거나 영구적인 파일을 생성하기 전에 데이터를 테스트하는 용도로 사용될 수 있습니다. 

## 사용법:
임시 파일을 생성하는 방법에 대한 코딩 예제와 출력 예시입니다. 

```C 
#include <stdio.h>
#include <stdlib.h>

int main() {

    // 임시 파일 생성
    FILE* tmp_file = tmpfile();

    // 임시 파일에 텍스트 쓰기
    fprintf(tmp_file, "Hi there!");

    // 임시 파일에서 텍스트 읽기
    char buffer[30];
    fseek(tmp_file, 0, SEEK_SET);
    fgets(buffer, 30, tmp_file);

    printf("임시 파일에서 읽은 내용: %s\n", buffer);

    // 임시 파일 닫기
    fclose(tmp_file);

    return 0;
}
```

출력:
```
임시 파일에서 읽은 내용: Hi there!
```

## 깊게 들어가기:
(1) 임시 파일의 역사적 배경 (2) 대안 (3) 생성 과정에 대한 구현 세부 사항 등, 임시 파일 생성에 대한 더 깊은 정보를 제공합니다. 

임시 파일은 수십 년 전부터 사용되어 왔습니다. 이전 시스템에서는 임시 파일을 생성하기 위해 디스크 공간을 사용하였지만, 최근 시스템에서는 메모리를 활용하여 더 빠르고 효율적으로 임시 파일을 생성할 수 있게 되었습니다. 

다른 대안으로는 메모리 버퍼를 사용하는 방법이 있습니다. 그러나 이는 임시 파일을 생성할 때와 동일한 목적을 달성하기 어렵고, 그렇기 때문에 여전히 임시 파일을 사용하는 경우가 많습니다. 

임시 파일 생성은 시스템의 파일 시스템을 사용하여 이루어집니다. 파일 시스템은 임시 파일을 위한 공간을 할당하고, 고유한 임시 파일 이름을 부여합니다. 또한, 임시 파일을 삭제하는 것도 파일 시스템의 역할입니다. 

## 참고 자료:
임시 파일 생성에 대한 관련 자료를 제공합니다. 

- [man7.org의 tmpfile() 문서](https://man7.org/linux/man-pages/man3/tmpfile.3.html)
- [TutorialsTeacher에서의 임시 파일 생성 방법 설명](https://www.tutorialsteacher.com/c/c-tmpfile)
- [Baeldung의 임시 파일 생성 가이드](https://www.baeldung.com/java-edit-temporary-file)