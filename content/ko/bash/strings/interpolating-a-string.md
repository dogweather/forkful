---
date: 2024-01-20 17:50:36.937936-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBCC0\uC218\uB098 \uD45C\uD604\
  \uC2DD\uC758 \uAC12\uC744 \uBB38\uC790\uC5F4 \uC548\uC73C\uB85C \uC0BD\uC785\uD558\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574 \uCF54\uB4DC\uC758 \uC720\
  \uC5F0\uC131\uC744 \uB192\uC774\uACE0, \uB3D9\uC801\uC73C\uB85C \uB370\uC774\uD130\
  \uB97C \uBB38\uC790\uC5F4\uC5D0 \uACB0\uD569\uD560 \uC218 \uC788\uAE30 \uB54C\uBB38\
  \uC5D0 \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.458934-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBCC0\uC218\uB098 \uD45C\uD604\uC2DD\
  \uC758 \uAC12\uC744 \uBB38\uC790\uC5F4 \uC548\uC73C\uB85C \uC0BD\uC785\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (방법)
```Bash
# 변수 보간 사용 예
name="세종대왕"
greeting="안녕하세요, $name님!"
echo $greeting # 출력: 안녕하세요, 세종대왕님!

# 명령어 실행 결과를 문자열에 보간
user_count=$(who | wc -l)
echo "접속 중인 사용자 수: $user_count" # 출력: 접속 중인 사용자 수: 3
```

## Deep Dive (심층 분석)
문자열 보간은 Bash에서 `" "` 안에서 `$변수명`을 사용하며 변수의 값을 문자열에 삽입합니다. 1990년대 초반 Bash는 Bourne Again SHell로서, Steve Bourne의 'sh'에 기반해 만들어진 Brian Fox에 의해 시작됐습니다. "$변수명" 대신에 `${변수명}`을 사용하는 이유는 문자열과 변수명을 명확히 구분하기 위함입니다. 예를 들어, "file${number}.txt" 처럼 사용할 수 있습니다. 하위 호환을 위해 꼭 필요하지 않은 한 `"`대신 `'`을 쓰는 것보다는 `"`을 사용하는 것이 좋습니다.

대체 방법으로는 `printf` 명령어나 Here document를 사용할 수도 있습니다. 예를 들면:

```Bash
# printf 명령어 사용
printf "안녕하세요, %s님!\n" "$name"

# Here document 사용
cat << END
안녕하세요, $name님!
END
```

## See Also (추가 자료)
- Bash Manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
