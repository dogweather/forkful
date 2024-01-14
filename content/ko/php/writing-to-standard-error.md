---
title:    "PHP: 표준 에러 기록하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 *표준 오류*를 작성해야 할까요?

프로그래밍을 하다보면 가끔 예상하지 못한 오류가 발생할 수 있습니다. 이 때, 우리는 오류를 추적하고 디버깅하기 위해 많은 시간을 소비합니다. 하지만 이러한 과정을 좀 더 수월하게 만들 수 있는 방법이 있습니다. 바로 *표준 오류*를 작성하는 것입니다. 이를 통해 우리는 오류가 발생한 부분을 쉽게 찾고, 정확한 원인을 파악할 수 있습니다.

## 어떻게 *표준 오류*를 작성할까요?

우선, PHP에서는 `error_log()` 함수를 사용하여 *표준 오류*를 작성할 수 있습니다. 이 함수는 다음과 같은 형태로 사용할 수 있습니다.

```PHP
error_log(message, message_type, destination);
```

`message`는 작성할 오류 메시지를 의미하며, 문자열 형태로 입력해야 합니다. `message_type`은 오류 메시지의 유형을 지정하는 매개변수입니다. 기본값은 `0`이며, 이는 `php.ini`에서 설정한 `error_reporting()` 값에 따라 결정됩니다. `destination`은 오류 메시지를 저장할 파일의 경로를 의미합니다. 기본값은 `php.ini`에서 설정한 `error_log()` 값에 따라 결정됩니다.

위의 매개변수를 커스터마이징하여 `error_log()` 함수를 사용하면, 오류 발생 시 원하는 위치에 오류 메시지를 작성할 수 있습니다. 또한, `echo`나 `print` 등의 출력문을 사용하여 원하는 위치에 오류 메시지를 출력할 수도 있습니다.

## 깊이있게 알아보기

*표준 오류*는 오류 발생 시 우리에게 많은 도움을 줍니다. 하지만 이를 제대로 활용하기 위해선 몇 가지 주의할 점이 있습니다.

먼저, 오류 메시지의 내용은 가능하면 간결하게 작성하는 것이 좋습니다. 오류의 원인을 파악할 수 있는 정보를 포함하면 됩니다. 또한, 너무 자주 *표준 오류*를 작성하면 모니터링이 어려워질 수 있으므로, 적절한 타이밍과 빈도로 작성하는 것이 중요합니다.

또한, *표준 오류*를 작성하는 것은 애플리케이션이 성공적으로 실행되는지를 확인하는 데에도 도움이 됩니다. 예를 들어, `error_log()` 함수를 사용하여 프로그램의 각 단계에서 발생한 오류를 확인할 수 있습니다. 이를 통해 애플리케이션을 디버깅하고, 개선할 수 있는 부분을 발견할 수 있습니다.

# 참고자료

- [PHP 공식 문서: error_log() 함수](https://www.php.net/manual/en/function.error-log.php)
- [How to Use PHP error_log to Log Errors to File](https://www.zentut.com/php-tutorial/php-error-log/)
- [PHP Debugging: How to Get Started and Properly Debug Your Code](https://www.codementor.io/blog/how-to-debug-php-error-log-du1089vp5)
- [Best Practices for Writing to Standard Error](https://www.linuxjournal.com/article/6487)