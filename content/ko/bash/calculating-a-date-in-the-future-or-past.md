---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Bash: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 무얼 & 왜?
날짜를 미래나 과거로 계산하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 날짜 계산은 특정한 날짜 간의 차이를 계산하는 것으로, 예를 들어 "내일"이나 "어제"와 같은 날짜를 다룰 때 유용합니다.

# 어떻게:
```Bash
# 현재 날짜에서 5일 전의 날짜 계산하기
date -d '5 days ago'
# 결과: Wed Jun 2 20:08:30 KST 2021
```

```Bash
# 30일 후의 날짜 계산하기
date -d '30 days'
# 결과: Fri Jul 2 20:09:58 KST 2021
```

```Bash
# 지난 주 일요일의 날짜 계산하기
date --date='last sunday'
# 결과: Sun Jun 13 00:00:00 KST 2021
```

# 깊게 들어가보면:
날짜 계산은 오래된 시스템과 프로그래밍 언어에서도 많이 사용되었습니다. 이전에는 C언어에서만 가능했지만, 현재는 Bash를 비롯한 다양한 언어에서도 사용할 수 있습니다. 날짜 계산 외에도 현재 시간과 날짜를 포맷팅하거나 시간대를 변경하는 등 다양한 기능을 제공합니다.

물론 날짜 계산을 위해서는 패키지나 라이브러리를 사용할 수도 있습니다. 하지만 좀 더 간단하고 빠른 방법으로 CLI(Command Line Interface)를 이용해 날짜 계산을 할 수 있습니다.

# 관련 자료:
- [GNU Coreutils 날짜 계산 문서](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html#Date-input-formats)
- [Bash 날짜 계산 예제 참고 사이트](https://linuxhint.com/bash-date-command/)
- [PHP 날짜/시간 함수들](https://www.php.net/manual/kr/ref.datetime.php)