---
title:                "PHP: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜 변경할까요?

날짜를 문자열로 변환하는 작업은 웹 개발에서 자주 사용되는 기술입니다. 예를 들어, 데이터베이스에서 날짜를 가져와서 사용자에게 보여줄 때, 문자열 형태로 변환하는 것이 편리합니다. 이를테면 "2020년 7월 15일" 형태로 변환하여 사용자가 쉽게 인식할 수 있게 할 수 있습니다.

# 어떻게 할까요?

```PHP
$date = date('Y-m-d'); // 현재 날짜를 가져옵니다.
$converted_date = strtotime($date); // 날짜를 타임스탬프 형식으로 변환합니다.
echo date('F j, Y', $converted_date); // 출력: July 15, 2020
```

위의 예시 코드를 보면, 먼저 현재 날짜를 가져오고, 이를 타임스탬프 형식으로 변환한 후 원하는 포맷에 맞게 다시 변환하여 출력하는 과정을 거칩니다. 이렇게 하면 날짜를 원하는 형태로 쉽게 변환할 수 있습니다.

# 딥 다이브

PHP의 `date()` 함수를 이용하여 날짜를 원하는 포맷으로 변환할 수 있습니다. 다만 이 함수는 날짜 포맷이나 시간대 등 설정을 잘 못하면 원하던 결과를 얻지 못할 수 있습니다. 따라서 `date()` 함수를 사용할 때는 원하는 포맷에 대한 충분한 이해와, 올바른 설정이 필요합니다. 또한, PHP의 `DateTime` 클래스를 이용하면 더욱 다양한 날짜 관련 기능을 사용할 수 있습니다.

# See Also

- PHP의 `date()` 함수: https://www.php.net/manual/kr/function.date.php
- PHP의 `DateTime` 클래스: https://www.php.net/manual/kr/class.datetime.php