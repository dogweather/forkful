---
title:    "PHP: 표준 오류에 쓰는 방법"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

표준 오류를 작성하는 것에 대해 이야기하는 이유는 코딩 과정에서 발생할 수 있는 문제를 식별하고 해결하는 데에 중요한 역할을 합니다. 특히 디버깅에 있어서 매우 유용한 방법이기 때문에 이를 잘 활용하는 것이 중요합니다.

## 사용 방법

```
PHP <?php
fwrite(STDERR, "This is an error message.");
```

위의 코드는 표준 오류에 메시지를 작성하는 가장 기본적인 방법입니다. 표준 오류는 터미널 창에서 `php` 명령어를 사용할 때 자동으로 표시되므로 디버깅을 할 때 유용합니다.

다음으로는, `error_log()` 함수를 사용하여 특정 파일에 오류 메시지를 작성하는 방법을 살펴보겠습니다.

```
PHP <?php
$error_message = "This is an error message.";
error_log($error_message, 3, "error_log.txt");
```

위의 코드에서 세 번째 매개변수는 오류 로그 파일의 경로를 나타내는데, 여기서는 `error_log.txt`로 설정하였습니다.

## 깊이 파고들기

표준 오류를 작성하는 것은 디버깅에 있어서 매우 유용하지만, 이를 너무 자주 사용하면 코드를 지저분하게 만들 수 있습니다. 따라서 오류 메시지를 사용할 때에는 최대한 명확하게 작성하는 것이 좋습니다.

또한, `try...catch` 구문을 사용하여 예외를 처리할 때는 `throw new Exception()`을 사용하여 표준 오류를 작성할 수도 있습니다.

```
PHP <?php
try {
  // something goes wrong
  throw new Exception("Something went wrong.");
}
catch(Exception $e) {
  fwrite(STDERR, $e->getMessage());
}
```

이렇게 작성하면 예외가 발생했을 때 표준 오류에 메시지를 작성할 수 있습니다.

## See Also

- [PHP Streams: Standard Streams](https://www.php.net/manual/en/features.commandline.io-streams.php)
- [PHP: error_log - Manual](https://www.php.net/manual/en/function.error-log.php)
- [PHP: throw - Manual](https://www.php.net/manual/en/language.exceptions.php#language.exceptions.throw)