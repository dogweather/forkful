---
title:    "PHP: 현재 날짜 받아오기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 현재 날짜를 가져오는 일에 참여하는 이유

현재 날짜를 가져오는 것은 프로그래밍에서 매우 일반적인 작업입니다. 날짜를 사용하여 다양한 기능을 구현하고, 사용자의 시간대에 맞게 표시하고, 이벤트를 설정하는 등 다양한 용도로 활용할 수 있기 때문입니다. 따라서 PHP 프로그래밍에 입문하는 사람들은 날짜를 제어하는 방법에 대해 알아야 합니다.

## 하는 방법:

PHP에서 현재 날짜를 가져오는 가장 간단한 방법은 `date()` 함수를 사용하는 것입니다. 이 함수는 파라미터에 형식을 지정하여 원하는 형식으로 날짜를 출력할 수 있습니다. 예를 들어, `date('Y-m-d')`와 같이 지정하면 현재 날짜를 년-월-일 형식으로 출력할 수 있습니다.

```
PHP에서 현재 날짜 가져오기 예제:

<?php
$date = date('Y-m-d');
echo $date; // 현재 날짜 출력 (예: 2021-07-08)
?>
```

또한 `time()` 함수를 사용하여 현재 시간을 가져올 수도 있습니다. 이 함수는 현재 시간을 초단위로 반환하므로, `date()` 함수와 함께 사용하여 원하는 형식으로 변환할 수 있습니다.

```
PHP에서 현재 시간 가져오기 예제:

<?php
$time = time();
echo date('H:i:s', $time); // 현재 시간 출력 (예: 14:20:33)
?>
```

## 깊게 들어가기:

PHP에서 현재 날짜를 가져오는 방법은 `date()` 함수를 이용하는 것 외에도 `DateTime` 클래스를 이용하여 보다 다양한 기능을 구현할 수 있습니다. 이 클래스는 날짜와 시간을 다루는 다양한 메서드를 제공하므로, 더 복잡한 날짜 관련 작업을 수행할 수 있습니다.

```
DateTime 클래스를 이용한 현재 날짜 가져오기 예제:

<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s'); // 현재 날짜와 시간 출력 (예: 2021-07-08 14:30:15)
?>
```

더 많은 정보는 공식 PHP 문서를 참고하시면 됩니다.

## 관련 링크:

- [PHP date() 함수 공식 문서](https://www.php.net/manual/ko/function.date.php)
- [PHP time() 함수 공식 문서](https://www.php.net/manual/ko/function.time.php)
- [PHP DateTime 클래스 공식 문서](https://www.php.net/manual/ko/class.datetime.php)