---
title:    "PHP: 날짜를 문자열로 변환하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# 왜

날짜를 문자열로 변환하는 것에 참여하는 이유는 다양합니다. 예를 들어, 데이터를 필터링하기 위해 날짜 범위를 비교해야 하는 경우, 형식이 일치하지 않으면 정확한 결과를 얻지 못할 수 있습니다.

# 어떻게

```PHP
<?php
$date = '2021-06-15'; //변환할 날짜
$newDate = date("Y/m/d", strtotime($date)); //입력된 날짜를 문자열로 변환
echo $newDate; //출력 결과: 2021/06/15
?>
```

위 예제는 날짜를 `Y/m/d` 형식의 문자열로 변환하는 방법을 보여줍니다. `strtotime()` 함수를 사용하여 입력된 날짜를 `date()` 함수에 전달해 필요한 형식으로 변환할 수 있습니다. 이 외에도 `DateTime` 클래스를 사용하여 날짜를 원하는 형식으로 변환할 수 있습니다.

# 딥 다이브

날짜를 문자열로 변환하는 것은 PHP에서 매우 일반적이고 유용한 작업입니다. 이를 통해 날짜를 원하는 형식으로 표현하고 필요에 따라 비교할 수 있습니다. 또한 PHP에서는 다양한 날짜 형식을 지원하기 때문에 필요한 경우 안전하게 사용할 수 있습니다.

# 참고 자료

[PHP Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)  
[Convert a date to a string in PHP](https://www.tutorialrepublic.com/php-tutorial/php-date-and-time.php)