---
title:                "Bash: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜
일시적인 파일을 만드는 것에 참여하는 이유는 무엇인가요? 일시적인 파일은 프로그램을 실행할 때 중요한 역할을 합니다. 이를 잘 이해하고 활용하는 것이 중요합니다.

## 어떻게
아래의 코드 블록을 사용하여 Bash 프로그래밍에서 일시적인 파일을 만드는 방법을 알아보겠습니다. 일시적인 파일을 만드는 가장 기본적인 방법은 'mktemp'를 사용하는 것입니다.

```Bash
# mktemp를 사용하여 일시적인 파일 만들기
temp_file=$(mktemp)
echo "일시적인 파일의 경로: $temp_file"
```
이를 실행하면 아래와 같은 결과를 얻을 수 있습니다.
```Bash
일시적인 파일의 경로: /tmp/tmp.gp51LoVbHf
```

만약 일시적으로 파일의 이름을 지정하고 싶다면, 아래와 같이 코드를 수정할 수 있습니다.

```Bash
# 일시적인 파일의 이름 지정하기
temp_file=$(mktemp temp_file_XXXXXX)
echo "일시적인 파일의 경로: $temp_file"
```
결과는 아래와 같이 출력됩니다.
```Bash
일시적인 파일의 경로: /tmp/temp_file_G1ULnC
```

이 외에도 'touch'나 'cp'와 같은 파일 관리 명령어를 사용하여 일시적인 파일을 만들 수 있습니다.

## 딥 다이브
일시적인 파일을 만드는 방법은 다양하지만 주로 사용하는 방법은 'mktemp'를 이용하는 것입니다. 'mktemp'는 사용자의 환경 변수를 이용하여 임시 파일을 생성하기 때문에 보안에 더 유리합니다. 또한 마지막 6자리는 랜덤한 숫자와 문자로 구성되어 있기 때문에 파일의 중복을 방지할 수 있습니다.

또한 일시적인 파일을 만들 때 중요한 점은 삭제하는 것입니다. 일시적인 파일은 일시적인 용도로 사용되기 때문에 사용이 끝나면 반드시 삭제해주어야 합니다. 'mktemp'를 사용할 경우 자동으로 삭제되지만, 직접 만들거나 관리 명령어를 사용할 경우에는 삭제에 신경써야 합니다.

# 참고자료
- [Bash 공식 문서](https://www.gnu.org/software/bash/)
- [mktemp man 페이지](https://www.man7.org/linux/man-pages/man1/mktemp.1.html)
- [Linuxize 블로그 - Bash에서 일시적인 파일 생성하기](https://linuxize.com/post/bash-temporary-file/)