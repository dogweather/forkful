---
title:                "Bash: 패턴과 일치하는 문자 삭제하기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?

"Pattern"와 일치하는 문자를 삭제하는 것이 왜 유용한지 궁금하신가요? 이 글에서는 Bash 프로그래밍에서 이 작업을 하는 이유와 방법에 대해 알아보겠습니다.

## 방법

Bash에서 "Pattern"과 일치하는 문자를 삭제하는 방법은 매우 간단합니다. 아래 코드 블록을 참고해보세요.

```Bash
# "Example"라는 파일에서 "Pattern"과 일치하는 문자 삭제하기
sed -i 's/Pattern//g' Example
```
위 코드를 실행하면 "Example" 파일에서 "Pattern"과 일치하는 문자가 모두 삭제됩니다. 아래는 삭제 전후의 예시 출력입니다.

**입력: Example 파일**
```Bash
# "HelloPattern, world!"
# "PatternDelete this line"
# "This line won't be affected"
```
**삭제 후 출력: Example 파일**
```Bash
# "Hello, world!"
# "Delete this line"
# "This line won't be affected"
```

## 깊게 파헤치기

자세한 내용은 [Bash 스크립트에서 sed 명령어를 사용하여 문자 삭제하기](https://www.linux.com/learn/using-sed-delete-empty-lines-file)라는 링크에서 확인할 수 있습니다. 이 글에서는 sed 명령어를 사용하는 다양한 방법과 실제 사용 예시를 다루고 있습니다.

## 참고자료

- [간결하고 강력한 Bash 스크립트 작성하기](https://linuxhint.com/write_useful_bash_script/)
- [Bash 프로그래밍을 위한 유용한 리소스 모음](https://wiki.bash-hackers.org/)
- [Bash 공식 문서 보기 (영문)](https://www.gnu.org/software/bash/manual/bash.html)

## 참조

위 글은 [주요 키워드] 글에서 참고한 내용을 바탕으로 작성되었습니다. 자세한 내용은 해당 글을 참고해주세요.