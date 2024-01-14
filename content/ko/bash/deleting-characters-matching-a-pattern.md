---
title:    "Bash: 패턴과 일치하는 문자 제거"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜

이번 포스트에서 우리는 Bash 프로그래밍에서 중요한 작업 중 하나인 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아볼 것입니다. 이 작업을 수행하는 이유는 사용자가 원하는 문자만을 남기고 나머지 문자를 제거해 정제된 데이터를 얻기 위해서입니다.

## 방법

우선, 삭제하고자 하는 문자가 포함된 문자열을 변수에 할당해야 합니다.

```Bash
str="Hello World!"
```

다음으로, `sed` 명령어를 사용하여 패턴과 일치하는 문자를 삭제합니다. 아래 예시에서는 공백 문자를 삭제하는 것을 보여드리겠습니다.

```Bash
new_str=$(echo $str | sed 's/ //g')
```

위 코드의 결과는 `HelloWorld!`가 됩니다.

만약 패턴과 일치하는 문자가 여러 개라면 `sed` 명령어 뒤에 `g` 옵션을 추가해야 모든 매칭된 문자를 삭제할 수 있습니다.

또 다른 방법으로는 `tr` 명령어를 사용하는 것입니다. `tr` 명령어는 전달된 문자를 다른 문자로 바꾸어주는 역할을 합니다. 따라서 아래 코드처럼 공백 문자를 빈 문자열로 바꾸어 문자를 삭제할 수 있습니다.

```Bash
new_str=$(echo $str | tr -d " ")
```

위 코드의 결과 역시 `HelloWorld!`가 됩니다.

## 깊이 파고들기

실제로는 더욱 복잡한 패턴을 다룰 수 있으며, `sed`와 `tr` 명령어 외에도 다양한 방법으로 패턴과 일치하는 문자를 삭제할 수 있습니다. 이를 위해 정규표현식이라는 강력한 도구를 사용할 수 있습니다. 정규표현식은 특정 패턴을 표현하는 문자열로, `grep` 명령어를 사용하여 문자열에서 정규표현식과 일치하는 부분을 찾을 수 있습니다. 그리고 이를 활용하여 패턴과 일치하는 문자를 삭제할 수 있습니다.

## 참고자료

- [The Linux Command Line: A Complete Introduction](https://linuxcommand.org/tlcl.php)
- [Bash Hackers Wiki](http://wiki.bash-hackers.org/)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [The Power of sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash Manual: tr](https://www.gnu.org/software/bash/manual/html_node/The-tr-Builtin.html)

# 참고자료