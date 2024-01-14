---
title:                "Bash: 디렉토리의 존재 여부 확인하기"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜
현재 작성되고 있는 이 블로그 포스트를 읽고 계신 여러분들은 이미 Bash 프로그래밍에 관심을 가지고 있거나 Bash를 배우고 있는 중이실 것입니다. 그리고 그 중에는 Bash에서 디렉토리의 존재 여부를 확인하는 방법에 대해 궁금해하는 분들도 있을 것입니다. 디렉토리의 존재 여부를 확인해야 하는 이유는, 예를 들어 프로그램을 실행하기 전에 해당 디렉토리가 존재하는지 확인하는 등의 상황에서 유용하게 쓰일 수 있기 때문입니다.

# 어떻게
Bash에서 디렉토리의 존재 여부를 확인하는 방법은 아주 간단합니다. 다음과 같이 코드를 작성해주면 됩니다.
```Bash
if [ -d "/경로/디렉토리" ] ; then
    echo "디렉토리가 존재합니다."
else
    echo "디렉토리가 존재하지 않습니다."
fi
```
위 코드에서는 `if`문과 `[` 부분이 디렉토리가 존재하는지를 확인하는 조건문입니다. `-d`는 디렉토리가 존재하면 참(true)을, 존재하지 않으면 거짓(false)을 반환합니다. 그리고 디렉토리의 경로는 `"/경로/디렉토리"` 부분에 지정하면 됩니다. `then` 다음에는 디렉토리가 존재할 경우에 실행할 코드를 작성하고, `else` 다음에는 존재하지 않을 경우에 실행할 코드를 작성하면 됩니다.

만약 여러분이 디렉토리가 존재할 경우에만 작업을 하고 싶을 때는 `if`문이나 `[` 부분을 제외한 코드만으로도 충분합니다. 다음과 같이 작성해주면 디렉토리가 존재할 경우에만 작업을 수행하게 됩니다.
```Bash
[ -d "/경로/디렉토리" ] && echo "디렉토리가 존재합니다."
```
또는 디렉토리가 존재하지 않을 경우에만 작업을 수행하고 싶다면 `&&` 대신 `||`를 사용하면 됩니다.

# 딥 다이브
더 깊게 들어가보면, Bash에서 디렉토리의 존재 여부를 확인하는 방법은 다양합니다. 예를 들어 `test` 명령어를 사용해도 디렉토리의 존재 여부를 확인할 수 있습니다. 또는 `if`문 대신 `[[`를 사용해도 동일한 결과를 얻을 수 있습니다. 또한 `help test`나 `help [[`를 입력하면 더 많은 옵션들을 확인할 수 있습니다.

# 관려 링크
## 참고 자료
- [Bash Guide for Beginners | 구루비](https://www.gurubee.net/article/81924)
- [Understanding Shell Scripting: Conditional Statements | Linux Academy](https://linuxacademy.com/blog/linux/understanding-shell-scripting-conditional-statements-part-1/)

## 원문 링크
- [Checking if a Directory Exists in Bash | Linuxize](https://linuxize.com/post/bash-check-if-directory-exists/)
- [Bash Guide for Beginners - Chapter 7 | 구루비](https://www.gurubee.net/article/54954)
- [7.3. 파일 속성 검사하기 | GNU C 프로그래밍 리퍼런스](https://www.gnu.org/software/libc/manual/html_node/Attribute-Tests.html)