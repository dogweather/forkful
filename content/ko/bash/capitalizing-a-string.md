---
title:                "Bash: 스트링 대문자 변환하기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 번째 글자를 대문자로 변경하는 과정은 프로그래밍에서 매우 유용합니다. 이 기능은 이름, 타이틀 등과 같이 첫 글자가 대문자로 시작해야 할 때 자주 사용됩니다.

## 진행 방법

[_capitalize_string.sh_](https://github.com/johndoe/capitalize_string.sh) 파일을 만들고 다음 코드를 추가하세요:

```Bash
# 변수에 문자열 할당
text="hello, world!"

# 문자열의 첫 번째 글자를 대문자로 변경
capitalized_text="$(tr '[:lower:]' '[:upper:]' <<< ${text:0:1})${text:1}"

# 결과 출력
echo "${capitalized_text}"
```

출력 결과는 다음과 같을 것입니다:

```
Hello, world!
```

이 코드의 첫 번째 줄에서 우리는 "hello, world!"라는 문자열을 `text` 변수에 할당합니다. 다음으로, 첫 번째 글자를 대문자로 변경하는 `tr` 명령어를 사용하여 `text` 변수 내의 문자열을 수정합니다. 마지막으로, 수정된 문자열을 `capitalized_text` 변수에 할당하고 `echo`를 사용하여 결과를 출력합니다.

## 더 깊이 들어가기

이제 문자열을 대문자로 변경하는 기능에 대해 조금 더 자세히 알아보겠습니다. Bash에서 문자열을 변경할 때 `tr` 명령어가 유용합니다. 이 명령어는 파일 또는 표준 입력에서 발견된 문자를 다른 문자로 바꿔줍니다. 위에서 본 예제에서 사용된 `tr` 명령어는 `[[:lower:]]` 패턴에 포함된 모든 소문자를 `[[:upper:]]` 패턴에 포함된 모든 대문자로 바꾸어 줍니다. 그리고 `<<<` 명령어는 표준 입력으로 문자열을 전달할 수 있게 해줍니다. 마지막으로, 변수명 다음에 `:`를 쓰고, 콜론 뒤의 숫자는 해당 변수의 몇 번째 문자부터 가져올지를 나타냅니다.

## 관련 링크

- [_bash(1) man page_](http://man7.org/linux/man-pages/man1/bash.1.html)
- [_tr(1) man page_](http://man7.org/linux/man-pages/man1/tr.1.html)
- [_Bash Pitfalls_](https://mywiki.wooledge.org/BashPitfalls)
- [_Shell Scripting Tutorial_](https://www.shellscript.sh/) 

## 참고
[_capitalize_string.sh_](https://github.com/johndoe/capitalize_string.sh) 파일의 내용을 수정하면 다른 문자열도 대문자로 변경할 수 있습니다. 또한 `tr` 명령어를 `sed`, `awk` 등 다른 명령어로 대체하여 같은 기능을 수행할 수도 있습니다. 자신만의 방식으로 코드를 작성해 보세요!