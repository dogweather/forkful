---
title:                "Bash: 정규 표현식 사용하기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규식을 사용해야 할까요? 

정규식은 문자열의 패턴을 찾고 매칭하며, 이를 사용하는 것은 프로그래밍에서 매우 유용합니다. 이를 통해 특정한 규칙이나 패턴을 가진 데이터를 쉽게 처리할 수 있으며, 코드의 유연성과 가독성을 높일 수 있습니다.

## 정규식 사용 방법

정규식은 Bash 프로그래밍에서 많이 사용되며, 아래 코드 블록을 통해 실제 코딩 예제를 살펴보겠습니다. 

```Bash
# 이메일 주소를 찾는 정규식 예제
STRING="제 이메일 주소는 abc123@example.com입니다."
EMAIL_REGEX="(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)"

if [[ $STRING =~ $EMAIL_REGEX ]]; then
  echo "이메일 주소를 찾았습니다: ${BASH_REMATCH[0]}"
fi
```

위의 코드는 "~" 연산자를 이용해 주어진 문자열에서 정규식과 일치하는지 확인하고, 일치하면 "BASH_REMATCH" 내장 변수를 통해 일치하는 문자열을 출력합니다. 이 외에도 정규식을 사용해 문자열을 추출하거나 변경할 수 있습니다.

## 정규식 깊이 파고들기

정규식은 다양한 패턴을 찾고 매칭하는 강력한 도구입니다. 따라서 정규식을 더 다양한 방법으로 활용할 수 있도록 깊이 공부하는 것이 중요합니다. 예를 들어, 대소문자를 구분하지 않고 일치하는 정규식을 작성하거나, 그룹을 이용해 일치하는 문자열의 일부분만 추출하는 방법 등을 배울 수 있습니다.

## 더 알아보기 

- [Bash에서의 정규식 사용 예제](https://www.ibm.com/developerworks/library/l-using-regular-expressions/)
- [정규식 기초 강의 영상](https://www.youtube.com/watch?v=rhzKDrUiJVk)
- [Bash 정규식 패턴 목록](https://www.linuxjournal.com/content/bash-extended-globbing)
- [Bash 정규식 문서](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html#Pattern-Matching)