---
title:    "Bash: 임시 파일 만들기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
가끔 Bash 쉘 스크립트를 작성할 때 임시 파일을 만들어야 할 때가 있습니다. 이는 보통 데이터를 처리하거나 시스템 리소스를 관리하는 프로세스 중간에 필요한 작은 공간을 제공하기 위해서입니다.

## 사용 방법
Bash 쉘에서 명령을 실행할 때 임시 파일을 만드는 가장 쉬운 방법은 `mktemp` 명령어를 사용하는 것입니다. `mktemp`는 임시 파일의 이름을 생성하고 해당 파일을 생성하는 데 사용됩니다. 아래는 `mktemp`를 사용해 임시 파일을 생성한 후 파일을 조작하는 간단한 예제입니다.

```Bash
# 임시 파일 생성
tempfile=$(mktemp)

# 임시 파일에 텍스트 쓰기
echo "안녕하세요! 이 임시 파일은 Bash 프로그래밍을 위해 생성되었습니다." > $tempfile

# 파일 내용 출력
cat $tempfile

# 임시 파일 삭제
rm $tempfile
```

결과는 아래와 같이 나올 것입니다.

```
안녕하세요! 이 임시 파일은 Bash 프로그래밍을 위해 생성되었습니다.
```

이 예제에서는 임시 파일을 생성하고 텍스트를 쓴 후 파일을 삭제했지만, 실제로 사용하는 경우에는 파일을 사용한 후에 삭제하는 것이 좋습니다. 그래야 다른 프로세스나 사용자가 임시 파일에 접근할 수 있습니다.

## 깊게 파고들기
`mktemp` 명령어는 매우 유용하지만, 더 깊이 파고드는 경우 `tempfile`이라는 임시 파일의 이름이 어떻게 정해지는지 궁금할 수 있습니다. 일반적으로 `mktemp`는 `/tmp` 디렉토리에 임시 파일을 생성하며 `tmp.XXXXXX`와 같은 형식으로 파일 이름을 만듭니다. 여기서 `XXXXXX`는 무작위로 생성된 문자열입니다. 이 문자열은 시스템 시간과 프로세스 ID를 기반으로 생성됩니다.

또 다른 방법으로는 `mktemp`의 인수에 원하는 파일 이름 형식을 지정할 수 있습니다. 예를 들어, `mktemp hello_XXXXXX` 명령을 실행하면 `hello_XXXXXX`와 같은 형식의 파일 이름을 가진 임시 파일이 생성됩니다.

또 다른 임시 파일 생성 방법으로는 `tmpfile=$(tempfile)`와 같은 명령을 사용하는 것입니다. 이 명령은 역시 임시 파일의 이름을 생성하고 해당 파일을 생성합니다. 다만 `tempfile`은 더 이상 `mktemp`와 같은 방식으로 생성된 이름이 아니라 `tmp.`와 같은 접두사를 가지고 있습니다.

## 그 밖의 정보
* `mktemp` 명령어는 `--help` 옵션을 사용해 관련된 정보를 확인할 수 있습니다.
* `mktemp`는 기본적으로 임시 파일의 보안 퍼미션을 `0600`으로 설정합니다. 이를 변경하고 싶은 경우에는 `mktemp -m` 옵션을 사용할 수 있습니다.

## 관련 자료
* [Bash 쉘 스크립트 튜토리얼](https://www.linode.com/docs/guides/bash-scripting/)
* [우분투 공식 도큐먼트 (한국어)](https://wiki.ubuntu-kr.org)
* [Linux 임시 파일 관련 정보 (영어)](https://www.cyberciti.biz/faq/bash-temporary-file/)

## 참고
이 글은 Bash 쉘