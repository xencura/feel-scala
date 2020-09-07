/*
 * Copyright Camunda Services GmbH and/or licensed to Camunda Services GmbH
 * under one or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information regarding copyright
 * ownership. Camunda licenses this file to you under the Apache License,
 * Version 2.0; you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.feel.impl.builtin

import java.time.{LocalDate, LocalTime, ZoneId, ZonedDateTime}

import org.camunda.feel.impl.FeelIntegrationTest
import org.camunda.feel.syntaxtree.{ValDate, ValDateTime}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class BuiltinTemporalFunctionsTest
    extends FlatSpec
    with Matchers
    with FeelIntegrationTest
    with BeforeAndAfter {

  private val now = ZonedDateTime.of(
    LocalDate.parse("2020-07-31"),
    LocalTime.parse("14:27:30"),
    ZoneId.of("Europe/Berlin")
  )

  "The now() function" should "return the current date-time" in withClock {
    clock =>
      clock.currentTime(now)
      eval(""" now() """) should be(ValDateTime(now))
  }

  "The today() function" should "return the current date" in withClock {
    clock =>
      clock.currentTime(now)
      eval(""" today() """) should be(ValDate(now.toLocalDate))
  }

}
