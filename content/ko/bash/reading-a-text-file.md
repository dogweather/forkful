---
title:                "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

쉘 스크립트를 배우는 한국어 독자들에게는 "텍스트 파일 읽기"가 처음 접하는 개념일 수도 있습니다. 이 글을 통해 쉘 스크립트에서 텍스트 파일을 읽는 법을 배우고, 어떤 경우에 이를 활용할 수 있는지 알아보겠습니다.

# 어떻게

우선 텍스트 파일을 읽는 가장 기본적인 방법은 `cat`을 이용하는 것입니다. 다음과 같은 코드 블록을 통해 예제를 살펴보겠습니다.

```Bash
#!/bin/bash

# hello.txt 파일을 읽어서 내용을 출력합니다.
cat hello.txt
```

위 코드를 실행하면 `hello.txt` 파일에 있는 내용이 출력되게 됩니다. 다만, `cat`은 파일 내용을 그대로 출력하는 것이기 때문에 라인별로 내용을 읽을 수 없습니다.

이런 경우 `while` 루프를 사용하면 파일 내용을 한 줄씩 읽을 수 있습니다. 다음 코드를 살펴보겠습니다.

```Bash
#!/bin/bash

# hello.txt 파일을 한 줄씩 읽어서 변수에 저장하고 출력합니다.
while read line
do
  echo "$line"
done < hello.txt
```

위 코드에서 `while` 루프를 이용해 파일 내용을 한 줄씩 읽고, `echo`를 이용해 해당 줄을 출력하는 것을 볼 수 있습니다. 이렇게 하면 파일 내용을 라인별로 읽을 수 있게 됩니다.

# 깊게 파고들기

텍스트 파일을 읽는 것은 쉘 스크립트에서 매우 중요한 기능입니다. 파일 내용을 읽어오는 것 뿐만 아니라, 해당 내용을 활용해 다양한 작업을 할 수 있기 때문입니다.

가령, 파일 내용을 읽어와서 조건문을 통해 특정 단어가 포함되어 있는지 확인하고, 포함되어 있다면 특정 작업을 수행하도록 할 수 있습니다. 또한 읽은 내용을 변수에 저장하고, 이를 다른 변수나 파일에 활용할 수도 있습니다.

텍스트 파일을 읽는 것은 쉘 스크립트에서의 가장 기본적인 작업 중 하나이기 때문에, 적극적으로 활용해보시기 바랍니다.

# 관련 링크

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [리눅스 쉘 스크립트 기초 입문](https://www.dreamy.pe.kr/zbxe/CodeClip/11146172)