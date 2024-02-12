---
title:                "테스트 작성하기"
aliases:
- /ko/php/writing-tests/
date:                  2024-02-03T19:31:29.867020-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
프로그래밍에서 테스트를 작성하는 것은 다양한 조건에서 코드가 예상대로 동작하는지 확인하기 위해 스크립트를 만들고 실행하는 과정을 말합니다. 프로그래머들은 이를 통해 품질을 보장하고, 회귀를 방지하며, 안전한 리팩토링을 용이하게 하여 건강하고, 확장 가능하며, 버그가 없는 코드베이스를 유지하는 데 중요합니다.

## 어떻게:
### 네이티브 PHP – PHPUnit
PHP에서 테스트를 위해 널리 사용되는 도구는 PHPUnit입니다. Composer를 통해 설치하세요:
```bash
composer require --dev phpunit/phpunit ^9
```

#### 간단한 테스트 작성하기:
`tests` 디렉토리에 `CalculatorTest.php` 파일을 생성하세요:
```php
use PHPUnit\Framework\TestCase;

// 숫자를 더하는 Calculator 클래스가 있다고 가정
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
다음으로 테스트를 실행하세요:
```bash
./vendor/bin/phpunit tests
```

#### 샘플 출력:
```
PHPUnit 9.5.10 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.005, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

### 제3자 라이브러리 – Mockery
모킹 객체를 포함한 복잡한 테스트의 경우, Mockery가 인기 있는 선택입니다.

```bash
composer require --dev mockery/mockery
```

#### Mockery를 PHPUnit과 통합하기:
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
실행할 때는 위의 PHPUnit 명령과 같이 사용하세요. Mockery는 표현력이 뛰어나고 유연한 모킹 객체를 가능하게 하여 애플리케이션 내 복잡한 상호작용을 테스트하는 데 도움을 줍니다.
