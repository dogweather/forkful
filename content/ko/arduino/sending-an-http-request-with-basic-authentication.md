---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Arduino: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

우리는 프로그램을 작성할 때 종종 다른 서버 또는 웹사이트와 통신해야 할 때가 있습니다. 이 때 가장 일반적인 방법 중 하나는 HTTP 요청을 보내는 것입니다. 하지만 때로는 이 요청에 추가적인 보안이 필요할 수 있습니다. 이때 자주 사용되는 방법 중 하나가 바로 "기본 인증"이라는 방법입니다. 이 방법을 사용하면 요청을 전송하는 프로그램이나 장치의 신원을 확인할 수 있습니다.

## 무엇인가요? 왜 사용할까요?
"기본 인증"은 단순히 말해서 서버로 요청을 보낼 때 인증 정보를 포함시키는 것입니다. 이는 보안을 강화하는 한 가지 방법입니다. 프로그래머들은 이 방법을 사용하여 요청을 전송하는 장치나 프로그램의 신원을 확인할 수 있습니다.

## 사용 방법:
"기본 인증"을 사용하는데는 몇 가지 단계가 필요합니다.

1. 먼저 우리는 HTTPClient 라이브러리를 사용하여 원하는 주소의 클라이언트를 만듭니다. 예를 들어, "www.example.com" 주소의 클라이언트를 만들려면 다음과 같이 작성합니다.

```Arduino
HTTPClient http;
http.begin("http://www.example.com");
```

2. 다음으로 인증 정보를 추가해야 합니다. 이를 위해 setAuthorization 메서드를 사용하며, 첫 번째 매개변수로는 사용자 이름을, 두 번째 매개변수로는 비밀번호를 입력합니다.

```Arduino
http.setAuthorization("username", "password");
```

3. 이제 우리가 원하는 작업을 수행하고 결과를 받아올 수 있습니다. 예를 들어, 이메일을 보내는 프로그램을 만든다면 다음과 같이 작성할 수 있습니다.

```Arduino
http.POST("email content");
```

4. 마지막으로, 사용이 끝나면 반드시 연결을 종료해야 합니다. 종료되지 않으면 연결이 계속 유지되며 프로그램이 제대로 작동하지 않을 수 있습니다.

```Arduino
http.end();
```

## 깊이 파고들기:
"기본 인증"은 HTTP 요청의 일부분으로 1999년에 처음 정의되었습니다. 이는 네트워크 보안을 강화하기 위해 개발되었으며, 현재도 널리 사용되고 있습니다. 하지만 이 방법은 보안이 강력하지 않아서 중요한 정보를 주고받을 때는 더 안전한 대체 방법을 고려해야 합니다.

Arduino에서 HTTP 요청을 보내는데 "기본 인증"을 사용하는 방법 외에도 다른 방법들이 있습니다. GET 요청을 보낼 때 헤더에 인증 정보를 추가할 수도 있으며, HTTPS를 사용할 수도 있습니다. HTTPS를 사용하면 암호화 된 연결을 제공하여 더 강력한 보안을 제공합니다.

## 더 알아보기:
다양한 인증 방법을 다루는 샘플 코드 및 자세한 설명은 아래의 링크에서 확인할 수 있습니다.

- [Basic Authentication with HTTPClient library](https://github.com/arduino-libraries/ArduinoHttpClient/blob/master/examples/BasicAuthentication/BasicAuthentication.ino)
- [ArduinoHttpClient library documentation](https://github.com/arduino-libraries/ArduinoHttpClient)