---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:36:12.424219-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자열에서 날짜 파싱은 문자열 형태의 데이터로부터 날짜 형식을 추출하는 과정입니다. 프로그래머들은 데이터 소프트웨어 처리에서 날짜 관련 연산을 하기 위해 파싱을 수행합니다.

## How to (어떻게 하나)
Fish Shell에서 문자열로부터 날짜를 파싱하려면 `strptime` 함수를 사용하세요. 아래는 예시 코드와 출력 결과입니다.

```Fish Shell
# 날짜 문자열 파싱하기
set date_string "2023-03-14 09:26:53"
set format "%Y-%m-%d %H:%M:%S"
set epoch_time (date --date="$date_string" "+%s")

# 결과 출력
echo $epoch_time
```

이 예제에서는 "2023-03-14 09:26:53"라는 문자열을 Unix epoch time으로 변환합니다. 출력 결과는 해당 날짜의 epoch 시간입니다.

## Deep Dive (심층 분석)
Fish Shell에서의 날짜 파싱은 다른 쉘과 비교했을 때 비교적 직관적입니다. 예전에는 더 복잡한 절차나 외부 도구가 필요했지만, 현재는 내장된 `date` 명령을 통해 문자열에서 날짜로의 변환이 가능합니다. 대안으로는 `strftime` 함수를 사용하여 날짜 형식을 문자열로 변환할 수 있으며 양방향의 파싱이 가능합니다. 구현 세부 사항으로는 `date` 명령이 시스템의 `date` 명령을 래핑(wrapping)하여 사용한다는 점입니다. 이는 사용환경의 `date` 명령에 의존하는 바, 다른 환경에서도 동일한 결과를 보장하기 위해서는 주의가 필요합니다.

## See Also (관련 자료)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils Date Documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [strftime and strptime Behavior](https://man7.org/linux/man-pages/man3/strftime.3.html)
