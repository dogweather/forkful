---
title:                "날짜를 문자열로 변환하기"
html_title:           "Bash: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 작업을 하려면 왜할까요? 날짜 정보를 다양한 형식으로 표현하고 다루기 위해 문자열로 변환하는 것은 유용합니다.

## 하는 법

```Bash
#!/bin/bash
# 현재 날짜를 문자열로 변환하는 예제

# 오늘의 날짜 정보를 yyyy-mm-dd 형식으로 저장
date=$(date '+%Y-%m-%d')

# 출력
echo "오늘은 $date 입니다."
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다: 
```
오늘은 2020-11-18 입니다.
```

## 깊게 파헤치기

날짜를 문자열로 변환하는 데는 다양한 방법이 있습니다. 일반적으로 `date` 명령어를 사용하여 현재 날짜 정보를 얻은 후, `date` 포맷 옵션을 사용하여 원하는 형식을 지정할 수 있습니다. 이 외에도 `printf` 함수를 사용하여 날짜 정보를 특정한 형식으로 출력할 수도 있습니다.

## 참고자료

- [Linux Command - date](https://www.geeksforgeeks.org/date-command-linux-examples/)
- [Bash Shell - printf](https://linuxhint.com/printf_in_bash_shell_script/)