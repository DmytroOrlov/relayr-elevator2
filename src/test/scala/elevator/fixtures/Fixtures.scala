package elevator.fixtures

import distage.plugins.PluginDef
import izumi.distage.effect.modules.ZIODIEffectModule
import izumi.functional.bio.BIO
import org.scalacheck.Gen.Parameters
import org.scalacheck.{Arbitrary, Prop}
import zio._

object TestEffectPlugin extends PluginDef with ZIODIEffectModule
