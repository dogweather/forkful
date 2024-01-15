---
title:                "날짜를 문자열로 변환하기"
html_title:           "PHP: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜 디레이트를 문자열로 변환하는가?

날짜를 문자열로 변환하는 이유는 때때로 우리가 보고 있는 정보를 표시하거나 포맷팅 할 때 필요할 수 있기 때문입니다.

## 어떻게 하나요?

날짜를 문자열로 변환하는 가장 간단한 방법은 PHP의 `date()` 함수를 사용하는 것입니다. 이 함수는 날짜와 시간 포맷을 지정할 수 있으며, 문자열로 변환된 결과를 반환합니다. 예를 들어:

```PHP
$currentDate = date('F j, Y');
```

위의 예제에서 `F`는 월 이름, `j`는 일 숫자, `Y`는 4자리 연도를 나타낸 것입니다. 이와 같은 방식으로 원하는 날짜 형식을 지정할 수 있습니다. 다양한 포맷 옵션에 대해 더 알아보려면 PHP 공식 문서를 참조하시기 바랍니다.

문자열로 변환된 날짜를 확인하려면 `echo`를 사용하여 출력하면 됩니다.

```PHP
echo $currentDate; // 출력 결과: April 28, 2021
```

## 깊이 파보기

PHP에서는 날짜를 문자열로 변환하는 데 사용할 수 있는 다양한 함수들이 있습니다. `date()` 함수 외에도 `DateTime` 클래스를 사용할 수 있고, `strftime()` 함수를 사용하여 로케일을 고려한 문자열 포맷팅을 할 수도 있습니다.

또한 `time()` 함수를 사용하여 현재 시간을 나타내는 타임스탬프를 받을 수 있으며, 이를 `date()` 함수에 입력하여 원하는 포맷으로 변환할 수 있습니다.

더 자세한 정보와 예제 코드는 PHP 공식 문서를 확인해보시기 바랍니다.

## 관련 링크

- [PHP `date()` 함수 문서](https://www.php.net/manual/en/function.date.php)
- [PHP `DateTime` 클래스 문서](https://www.php.net/manual/en/class.datetime.php)
- [PHP `strftime()` 함수 문서](https://www.php.net/manual/en/function.strftime.php)
- [PHP `time()` 함수 문서](https://www.php.net/manual/en/function.time.php)
- [PHP 날짜/시간 관련 함수 및 클래스 전체 목록](https://www.php.net/manual/en/ref.datetime.php)