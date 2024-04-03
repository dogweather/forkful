---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:29.867020-07:00
description: "\uC5B4\uB5BB\uAC8C: #."
lastmod: '2024-03-13T22:44:55.361689-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

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
