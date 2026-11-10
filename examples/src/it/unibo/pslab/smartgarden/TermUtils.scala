package it.unibo.pslab.smartgarden

object TermUtils:
  private val Esc = "\u001b["
  def reset = s"${Esc}0m"
  def paint(code: Int)(s: String) = s"$Esc${code}m$s$reset"
  def bold(s: String) = paint(1)(s)
  def dim(s: String) = paint(2)(s)
  def red(s: String) = paint(31)(s)
  def green(s: String) = paint(32)(s)
  def yellow(s: String) = paint(33)(s)
  def cyan(s: String) = paint(36)(s)

  private val ansi = "\u001b\\[[0-9;]*m".r
  def visibleLength(s: String): Int = ansi.replaceAllIn(s, "").length
  def pad(s: String, width: Int): String = s + " " * (width - visibleLength(s)).max(0)

  /** Rounded panel with a title; width adapts to the content. */
  def panel(title: String, lines: List[String], minWidth: Int = 46): String =
    val inner = (lines.map(visibleLength) :+ (visibleLength(title) + 4) :+ minWidth).max
    val top = s"╭─ ${bold(title)} ${"─" * (inner - visibleLength(title) - 1)}╮"
    val body = lines.map(l => s"│ ${pad(l, inner)} │")
    val bottom = s"╰${"─" * (inner + 2)}╯"
    (top :: body ::: List(bottom)).mkString("\n")

  /** Horizontal gauge: ▕████████░░░░░░░░░░░░▏ 42% */
  private def levelColor(r: Double): String => String =
    if r <= 0.4 then green else if r < 0.8 then yellow else red

  def gauge(ratio: Double, width: Int = 20): String =
    val r = ratio.max(0.0).min(1.0)
    val filled = (r * width).round.toInt
    val bar = levelColor(r)("█" * filled) + dim("░" * (width - filled))
    f"▕$bar▏ ${r * 100}%3.0f%%"

  def status(r: Double): String =
    if r <= 0.4 then green("● LOW")
    else if r < 0.8 then yellow("● MEDIUM")
    else red("● HIGH")
